const std = @import("std");
const c = @import("common.zig");
const VM = @import("vm.zig").VM;

pub const Obj = struct {
    pub const Type = enum { String };

    pub const String = struct {
        obj: Obj,
        str: []u8,

        pub fn as_obj(self: *String) *Obj {
            return &self.obj;
        }

        pub fn deinit(self: *String, vm: *VM) void {
            vm.allocator.free(self.str);
            vm.allocator.destroy(self);
        }
    };

    type: Type,
    next: ?*Obj = null,

    pub fn is_string(self: *Obj) bool {
        return self.type == .String;
    }

    pub fn as_string(self: *Obj) *String {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub fn deinit(self: *Obj, vm: *VM) void {
        switch (self.type) {
            .String => self.as_string().deinit(vm),
        }
    }
};

pub fn copy_string(str: []const u8, vm: *VM) !*Obj.String {
    const copy = try vm.allocator.alloc(u8, str.len);
    std.mem.copyForwards(u8, copy, str);
    return alloc_string(copy, vm);
}

pub fn take_string(str: []u8, vm: *VM) !*Obj.String {
    return alloc_string(str, vm);
}

fn alloc_string(str: []u8, vm: *VM) !*Obj.String {
    var obj = try alloc_obj(.String, vm);
    var str_obj = obj.as_string();
    str_obj.str = str;

    return str_obj;
}

fn alloc_obj(obj_type: Obj.Type, vm: *VM) !*Obj {
    const ptr = switch (obj_type) {
        .String => try vm.allocator.create(Obj.String),
    };
    ptr.obj = Obj{ .type = obj_type, .next = vm.objects };
    vm.objects = &ptr.obj;

    return ptr.as_obj();
}
