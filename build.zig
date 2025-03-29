// A script to build and test the zuo using Zig build system.
// The script matches build.zuo as much as possible.

const builtin = @import("builtin");
const std = @import("std");

const zig_min_required_version = "0.14.0";

comptime {
    const required_ver = std.SemanticVersion.parse(zig_min_required_version)
                            catch unreachable;
    if (builtin.zig_version.order(required_ver) == .lt) {
        @compileError(std.fmt.comptimePrint(
            "Zig version {} does not meet the build requirement of {}",
            .{ builtin.zig_version, required_ver },
        ));
    }
}

pub fn build(b: *std.Build) void {
    // Stage 0

    // Install lib/zuo to standard lib path.
    const lib = b.addInstallDirectory(.{
        .source_dir = b.path("lib/zuo"),
        .install_dir = .{ .lib = {} },
        .install_subdir = "zuo",
    });

    // zuo0 is only used to generate image_zuo.c file.

    // zuo0 is built to native target by default.
    // In some sepcial situation, user can set the target manually.
    const zuo0_target_option = b.option([]const u8, "zuo0-target",
        "zuo0 target (or else native)") orelse "native";
    const zuo0_query_result = std.Build.parseTargetQuery(.{
        .arch_os_abi = zuo0_target_option,
    }) catch unreachable;
    const zuo0_t = b.resolveTargetQuery(zuo0_query_result).result;

    const zuo0_linkage = b.option(std.builtin.LinkMode,
        "zuo0-linkage",
        "zuo0 link mode (default dynamic link)") orelse .dynamic;

    var escaped_path = std.ArrayList(u8).init(b.allocator);
    defer escaped_path.deinit();
    escapeWindowsPath(b.lib_dir, &escaped_path) catch unreachable;

    const zuo0 = b.addExecutable(.{
        .linkage = zuo0_linkage,
        .name = "zuo0",
        .root_module = b.createModule(.{
            .target = b.resolveTargetQuery(zuo0_query_result),
            .link_libc = true,
        }),
    });

    zuo0.addCSourceFile(.{
        .file = b.path("zuo.c"),
        .flags = &.{
            std.mem.concat(b.allocator, u8, &.{
                "-DZUO_LIB_PATH=",
                "\"",
                if (zuo0_t.os.tag == .windows) escaped_path.items
                else b.lib_dir,
                "\"",
            }) catch unreachable,
            if (zuo0_t.isMinGW()) "-D__MINGW32__"
            else if (zuo0_t.os.tag == .windows) "-D_MSC_VER=1700"
            else "",
        },
    });

    // Run local/image.zuo script to generate image_zuo.c file.
    const image_zuo = b.addRunArtifact(zuo0);
    image_zuo.addFileArg(b.path("local/image.zuo"));
    image_zuo.addArgs(&.{
        "-o", "image_zuo.c",
        "++lib", "zuo",
        "--keep-collects",
    });
    image_zuo.setCwd(b.path(""));
    image_zuo.step.dependOn(&lib.step);

    // Stage 1

    // Shared configuration for to-run zuo and to-install zuo.
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});
    const t = target.result;

    const linkage = b.option(std.builtin.LinkMode, "linkage",
        "Link mode (default dynamic link)") orelse .dynamic;
    const cflags_extra = b.option([]const u8, "cflags-extra",
        "Extra user-defined cflags") orelse "";

    const enable_werror = b.option(bool, "enable_werror",
        "Pass -Werror to the C compiler (treat warnings as errors)")
        orelse false;

    var source_files = std.ArrayList([]const u8).init(b.allocator);
    defer source_files.deinit();
    var flags = std.ArrayList([]const u8).init(b.allocator);
    defer flags.deinit();

    source_files.appendSlice(&.{
        "image_zuo.c",
    }) catch unreachable;

    if(t.isMinGW()) {
        flags.append("-D__MINGW32__") catch unreachable;
    } else if (t.os.tag == .windows) {
        // If the version of the Microsoft C/C++ compiler is 17.00.51106.1,
        // the value of _MSC_VER is 1700.
        // This maybe better then an arbitrary value.
        flags.append("-D_MSC_VER=1700") catch unreachable;
    }

    // Extra user-defined flags (if any) to pass to the compiler.
    if (cflags_extra.len > 0) {
        // Split it up on a space and append each part to flags separately.
        var tokenizer = std.mem.tokenizeScalar(u8, cflags_extra, ' ');
        while (tokenizer.next()) |token| {
            flags.append(token) catch unreachable;
        }
    }

    if (enable_werror) {
        flags.append("-Werror") catch unreachable;
    }

    // same as to-install zuo in build.zuo
    const zuo = b.addExecutable(.{
        .linkage = linkage,
        .name = "zuo",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });

    flags.append(std.mem.concat(b.allocator, u8, &.{
        "-DZUO_LIB_PATH=",
        "\"",
        if (zuo0_t.os.tag == .windows) escaped_path.items
        else b.lib_dir,
        "\"",
        }) catch unreachable
    ) catch unreachable;

    zuo.addCSourceFiles(.{
        .files = source_files.items,
        .flags = flags.items,
    });

    zuo.step.dependOn(&image_zuo.step);
    b.installArtifact(zuo);

    // same as to-run zuo in build.zuo
    const to_run = b.addExecutable(.{
        .linkage = linkage,
        .name = "zuo",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });

    // pop -DZUO_LIB_PATH flag for to-install zuo, share the other flags.
    _ = flags.pop();

    if (t.os.tag == .windows) {
        // DZUO_LIB_PATH is a C macro, there are double amount of separators.
        flags.append("-DZUO_LIB_PATH=\"..\\\\lib\"") catch unreachable;
    } else {
        flags.append("-DZUO_LIB_PATH=\"../lib\"") catch unreachable;
    }

    to_run.addCSourceFiles(.{
        .files = source_files.items,
        .flags = flags.items,
    });

    to_run.step.dependOn(&image_zuo.step);
    const to_run_install = b.addInstallArtifact(to_run, .{
        .dest_dir = .{.override = .{.custom = "to-run"}},
    });
    const to_run_step = b.step("to-run", "Build and install to-run zuo in build.zuo script");
    to_run_step.dependOn(&to_run_install.step);

    // Tests for to-install zuo.
    const test_step = b.step("test", "Run tests");

    const run_test_zuo = b.addRunArtifact(zuo);
    run_test_zuo.addFileArg(b.path("tests/main.zuo"));
    test_step.dependOn(&run_test_zuo.step);
}

fn escapeWindowsPath(path: []const u8, escaped: *std.ArrayList(u8)) !void {
    escaped.clearAndFree();
    for (path) |char| {
        if (char == '\\') {
            escaped.append('\\') catch unreachable;
            escaped.append('\\') catch unreachable;
        } else {
            escaped.append(char) catch unreachable;
        }
    }
    // Add trailing path separator.
    if (escaped.getLast() != '\\') {
        escaped.append('\\') catch unreachable;
        escaped.append('\\') catch unreachable;
    }
    return;
}
