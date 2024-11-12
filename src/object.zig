const std = @import("std");
const c = @import("common.zig");

pub const Obj = struct {
    pub const Type = enum { String };

    pub const String = struct {
        obj: Obj,
        str: []const u8,

        pub fn as_obj(self: *String) *Obj {
            return &self.obj;
        }
    };

    type: Type,

    pub fn is_string(self: *Obj) bool {
        return self.type == .String;
    }

    pub fn as_string(self: *Obj) *String {
        return @alignCast(@fieldParentPtr("obj", self));
    }
};

pub fn copy_string(str: []const u8, allocator: std.mem.Allocator) !*Obj.String {
    const copy = try allocator.alloc(u8, str.len);
    std.mem.copyForwards(u8, copy, str);
    return alloc_string(copy, allocator);
}

pub fn take_string(str: []u8, allocator: std.mem.Allocator) !*Obj.String {
    return alloc_string(str, allocator);
}

fn alloc_string(str: []u8, allocator: std.mem.Allocator) !*Obj.String {
    var obj = try alloc_obj(.String, allocator);
    var str_obj = obj.as_string();
    str_obj.str = str;

    return str_obj;
}

fn alloc_obj(obj_type: Obj.Type, allocator: std.mem.Allocator) !*Obj {
    const ptr = switch (obj_type) {
        .String => try allocator.create(Obj.String),
    };
    ptr.obj = Obj{ .type = obj_type };

    return ptr.as_obj();
}
