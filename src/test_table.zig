const std = @import("std");
const VM = @import("vm.zig").VM;
const objs = @import("object.zig");
const values = @import("values.zig");

const Table = @import("table.zig").Table;
const assert = std.debug.assert;

const allocator = std.testing.allocator;

test "add same key and get from table" {
    var vm = VM.init(allocator);
    defer vm.deinit();

    var table = Table.init(allocator);
    defer table.deinit();

    const k1 = try objs.copy_string("key1", &vm);

    const N = 100;
    for (0..N) |i| {
        const added = table.set(k1, .{ .number = @floatFromInt(i) });
        assert(added == (i == 0));
    }

    var result: values.Value = .nil;
    const found = table.get(k1, &result);

    assert(found);
    assert(@as(values.ValueType, result) == values.ValueType.number);
    assert(result.number == N - 1);
}

test "add different keys and get from table" {
    var vm = VM.init(allocator);
    defer vm.deinit();

    var table = Table.init(allocator);
    defer table.deinit();

    const N = 100;
    var keys = [_]*objs.Obj.String{undefined} ** N;
    for (0..N) |i| {
        const str = try std.fmt.allocPrint(allocator, "key-{d}", .{i});
        defer allocator.free(str);

        const key = try objs.copy_string(str, &vm);
        const added = table.set(key, .{ .number = @floatFromInt(i) });
        assert(added);

        assert(table.size() == (i + 1));

        var v: values.Value = undefined;
        const found = table.get(key, &v);
        assert(found);
        assert(values.is_number(v));
        assert(v.number == @as(f64, @floatFromInt(i)));

        keys[i] = key;
    }

    var v: values.Value = undefined;
    const magic = 37;
    const found = table.get(keys[magic], &v);

    assert(found);
    assert(values.is_number(v));
    assert(v.number == magic);
}

test "add different keys + delele" {
    var vm = VM.init(allocator);
    defer vm.deinit();

    var table = Table.init(allocator);
    defer table.deinit();

    const N = 100;
    var keys = [_]*objs.Obj.String{undefined} ** N;
    for (0..N) |i| {
        const str = try std.fmt.allocPrint(allocator, "key-{d}", .{i});
        defer allocator.free(str);

        const key = try objs.copy_string(str, &vm);
        const added = table.set(key, .{ .number = @floatFromInt(i) });
        assert(added);

        keys[i] = key;
    }

    for (0..N) |i| {
        const removed = table.delete(keys[i]);
        assert(removed);
    }

    assert(table.size() == 0);
}

test "init and de-init empty table" {
    var table = Table.init(std.testing.allocator);
    defer table.deinit();
}

test "search for keys in an empty table" {
    var vm = VM.init(allocator);
    defer vm.deinit();

    var table = Table.init(std.testing.allocator);
    defer table.deinit();

    const key = try objs.copy_string("n/a", &vm);
    var value: values.Value = undefined;
    const found = table.get(key, &value);
    assert(!found);
}
