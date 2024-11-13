const std = @import("std");
const VM = @import("vm.zig").VM;
const table = @import("table.zig");

pub const Obj = struct {
    pub const Type = enum { String };

    pub const String = struct {
        obj: Obj,
        str: []u8,
        hash: u32,

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
    const interned = table.find_string(&vm.strings, str, hash_u8(str));
    if (interned != null) {
        return interned.?;
    }

    const copy = try vm.allocator.alloc(u8, str.len);
    std.mem.copyForwards(u8, copy, str);
    return alloc_string(copy, vm, hash_u8(str));
}

pub fn take_string(str: []u8, vm: *VM) !*Obj.String {
    const interned = table.find_string(&vm.strings, str, hash_u8(str));
    if (interned != null) {
        vm.allocator.free(str);
        return interned.?;
    }

    return alloc_string(str, vm, hash_u8(str));
}

fn alloc_string(str: []u8, vm: *VM, hash: u32) !*Obj.String {
    var obj = try alloc_obj(.String, vm);
    var str_obj = obj.as_string();
    str_obj.str = str;
    str_obj.hash = hash;

    const added = vm.strings.set(str_obj, .nil);
    std.debug.assert(added);

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

fn hash_u8(str: []const u8) u32 {
    var result: u32 = 2166136261;
    for (str) |c| {
        result ^= c;
        result *%= 16777619;
    }

    return result;
}
