const Obj = @import("object.zig").Obj;
const values = @import("values.zig");
const std = @import("std");

pub const Entry = struct {
    key: ?*Obj.String,
    value: values.Value,
};

pub const Table = struct {
    allocator: std.mem.Allocator,
    entries: []Entry,
    total_count: u32 = 0,
    tombstones_count: u32 = 0,

    const INITIAL_CAPACITY = 16;
    const LOAD_FACTOR = 0.75;

    pub fn init(allocator: std.mem.Allocator) Table {
        return .{ .allocator = allocator, .entries = &[_]Entry{} };
    }

    pub fn deinit(self: *Table) void {
        self.allocator.free(self.entries);
    }

    pub fn set(self: *Table, key: *Obj.String, val: values.Value) bool {
        const threshold_count = @as(u32, @intFromFloat(@as(f64, @floatFromInt(self.capacity())) * LOAD_FACTOR));
        if (self.total_count + 1 > threshold_count) {
            self.increase_capacity() catch return false;
        }

        var entry = find_entry(self, key);
        const is_new_key = entry.key == null;
        if (is_new_key and !is_tombstone(entry)) {
            self.total_count += 1;
        }
        entry.key = key;
        entry.value = val;

        return is_new_key;
    }

    pub fn get(self: *Table, key: *Obj.String, val: *values.Value) bool {
        if (self.total_count == 0) return false;

        const entry = find_entry(self, key);
        if (entry.key != null) {
            val.* = entry.value;
            return true;
        }

        return false;
    }

    pub fn delete(self: *Table, key: *Obj.String) bool {
        const entry = find_entry(self, key);

        if (entry.key == key) {
            entry.key = null;
            entry.value = tombstone_value();
            self.tombstones_count += 1;

            return true;
        }

        return false;
    }

    pub fn size(self: *Table) u32 {
        return self.total_count - self.tombstones_count;
    }

    fn find_entry(self: *Table, key: *Obj.String) *Entry {
        var idx = key.hash % self.entries.len;

        while (true) {
            const entry = self.entries[idx];
            if (entry.key == key or (entry.key == null and !is_tombstone(&entry))) {
                return &self.entries[idx];
            }

            idx = (idx + 1) % self.entries.len;
        }
    }

    fn capacity(self: *Table) u32 {
        return @intCast(self.entries.len);
    }

    fn increase_capacity(self: *Table) !void {
        const new_capacity = if (self.capacity() < INITIAL_CAPACITY) INITIAL_CAPACITY else self.capacity() * 2;
        const new_entries = try self.allocator.alloc(Entry, new_capacity);

        for (new_entries) |*e| {
            e.key = null;
            e.value = .nil;
        }

        const old_entries = self.entries;
        self.entries = new_entries;

        self.total_count = 0;
        self.tombstones_count = 0;

        for (old_entries) |e| {
            if (e.key != null) {
                var new_e = find_entry(self, e.key.?);
                new_e.key = e.key;
                new_e.value = e.value;

                self.total_count += 1;
            }
        }

        self.allocator.free(old_entries);
    }

    fn is_tombstone(e: *const Entry) bool {
        return e.key == null and values.is_bool(e.value) and e.value.boolean;
    }

    fn tombstone_value() values.Value {
        return .{ .boolean = true };
    }
};

pub fn find_string(table: *Table, str: []const u8, hash: u32) ?*Obj.String {
    if (table.total_count == 0) return null;

    var idx = hash % table.entries.len;
    while (true) {
        const entry = table.entries[idx];
        if (entry.key) |key| {
            if (key.hash == hash and std.mem.eql(u8, key.str, str)) {
                return key;
            }
        } else if (!Table.is_tombstone(&entry)) {
            return null;
        }

        idx = (idx + 1) % table.entries.len;
    }
}
