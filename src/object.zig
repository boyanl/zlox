const c = @import("common.zig");

pub const Obj = struct {
    pub const Type = enum { String };

    pub const String = struct {
        obj: Obj,
        str: []u8,
    };

    type: Type,

    pub fn as_string(self: *Obj) *String {
        return @fieldParentPtr("obj", self);
    }
};
