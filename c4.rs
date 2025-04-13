#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]
extern "C" {
    fn printf(_: *const libc::c_char, _: ...) -> libc::c_int;
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn exit(_: libc::c_int) -> !;
    fn memset(
        _: *mut libc::c_void,
        _: libc::c_int,
        _: libc::c_ulong,
    ) -> *mut libc::c_void;
    fn memcmp(
        _: *const libc::c_void,
        _: *const libc::c_void,
        _: libc::c_ulong,
    ) -> libc::c_int;
    fn close(__fd: libc::c_int) -> libc::c_int;
    fn read(__fd: libc::c_int, __buf: *mut libc::c_void, __nbytes: size_t) -> ssize_t;
    fn open(__file: *const libc::c_char, __oflag: libc::c_int, _: ...) -> libc::c_int;
}
pub type size_t = libc::c_ulong;
pub type __ssize_t = libc::c_long;
pub type ssize_t = __ssize_t;
pub type C2RustUnnamed = libc::c_uint;
pub const Brak: C2RustUnnamed = 164;
pub const Dec: C2RustUnnamed = 163;
pub const Inc: C2RustUnnamed = 162;
pub const Mod: C2RustUnnamed = 161;
pub const Div: C2RustUnnamed = 160;
pub const Mul: C2RustUnnamed = 159;
pub const Sub: C2RustUnnamed = 158;
pub const Add: C2RustUnnamed = 157;
pub const Shr: C2RustUnnamed = 156;
pub const Shl: C2RustUnnamed = 155;
pub const Ge: C2RustUnnamed = 154;
pub const Le: C2RustUnnamed = 153;
pub const Gt: C2RustUnnamed = 152;
pub const Lt: C2RustUnnamed = 151;
pub const Ne: C2RustUnnamed = 150;
pub const Eq: C2RustUnnamed = 149;
pub const And: C2RustUnnamed = 148;
pub const Xor: C2RustUnnamed = 147;
pub const Or: C2RustUnnamed = 146;
pub const Lan: C2RustUnnamed = 145;
pub const Lor: C2RustUnnamed = 144;
pub const Cond: C2RustUnnamed = 143;
pub const Assign: C2RustUnnamed = 142;
pub const While: C2RustUnnamed = 141;
pub const Sizeof: C2RustUnnamed = 140;
pub const Return: C2RustUnnamed = 139;
pub const Int: C2RustUnnamed = 138;
pub const If: C2RustUnnamed = 137;
pub const Enum: C2RustUnnamed = 136;
pub const Else: C2RustUnnamed = 135;
pub const Char: C2RustUnnamed = 134;
pub const Id: C2RustUnnamed = 133;
pub const Loc: C2RustUnnamed = 132;
pub const Glo: C2RustUnnamed = 131;
pub const Sys: C2RustUnnamed = 130;
pub const Fun: C2RustUnnamed = 129;
pub const Num: C2RustUnnamed = 128;
pub type C2RustUnnamed_0 = libc::c_uint;
pub const EXIT: C2RustUnnamed_0 = 38;
pub const MCMP: C2RustUnnamed_0 = 37;
pub const MSET: C2RustUnnamed_0 = 36;
pub const FREE: C2RustUnnamed_0 = 35;
pub const MALC: C2RustUnnamed_0 = 34;
pub const PRTF: C2RustUnnamed_0 = 33;
pub const CLOS: C2RustUnnamed_0 = 32;
pub const READ: C2RustUnnamed_0 = 31;
pub const OPEN: C2RustUnnamed_0 = 30;
pub const MOD: C2RustUnnamed_0 = 29;
pub const DIV: C2RustUnnamed_0 = 28;
pub const MUL: C2RustUnnamed_0 = 27;
pub const SUB: C2RustUnnamed_0 = 26;
pub const ADD: C2RustUnnamed_0 = 25;
pub const SHR: C2RustUnnamed_0 = 24;
pub const SHL: C2RustUnnamed_0 = 23;
pub const GE: C2RustUnnamed_0 = 22;
pub const LE: C2RustUnnamed_0 = 21;
pub const GT: C2RustUnnamed_0 = 20;
pub const LT: C2RustUnnamed_0 = 19;
pub const NE: C2RustUnnamed_0 = 18;
pub const EQ: C2RustUnnamed_0 = 17;
pub const AND: C2RustUnnamed_0 = 16;
pub const XOR: C2RustUnnamed_0 = 15;
pub const OR: C2RustUnnamed_0 = 14;
pub const PSH: C2RustUnnamed_0 = 13;
pub const SC: C2RustUnnamed_0 = 12;
pub const SI: C2RustUnnamed_0 = 11;
pub const LC: C2RustUnnamed_0 = 10;
pub const LI: C2RustUnnamed_0 = 9;
pub const LEV: C2RustUnnamed_0 = 8;
pub const ADJ: C2RustUnnamed_0 = 7;
pub const ENT: C2RustUnnamed_0 = 6;
pub const BNZ: C2RustUnnamed_0 = 5;
pub const BZ: C2RustUnnamed_0 = 4;
pub const JSR: C2RustUnnamed_0 = 3;
pub const JMP: C2RustUnnamed_0 = 2;
pub const IMM: C2RustUnnamed_0 = 1;
pub const LEA: C2RustUnnamed_0 = 0;
pub type C2RustUnnamed_1 = libc::c_uint;
pub const PTR: C2RustUnnamed_1 = 2;
pub const INT: C2RustUnnamed_1 = 1;
pub const CHAR: C2RustUnnamed_1 = 0;
pub type C2RustUnnamed_2 = libc::c_uint;
pub const Idsz: C2RustUnnamed_2 = 9;
pub const HVal: C2RustUnnamed_2 = 8;
pub const HType: C2RustUnnamed_2 = 7;
pub const HClass: C2RustUnnamed_2 = 6;
pub const Val: C2RustUnnamed_2 = 5;
pub const Type: C2RustUnnamed_2 = 4;
pub const Class: C2RustUnnamed_2 = 3;
pub const Name: C2RustUnnamed_2 = 2;
pub const Hash: C2RustUnnamed_2 = 1;
pub const Tk: C2RustUnnamed_2 = 0;
#[no_mangle]
pub static mut p: *mut libc::c_char = 0 as *const libc::c_char as *mut libc::c_char;
#[no_mangle]
pub static mut lp: *mut libc::c_char = 0 as *const libc::c_char as *mut libc::c_char;
#[no_mangle]
pub static mut data: *mut libc::c_char = 0 as *const libc::c_char as *mut libc::c_char;
#[no_mangle]
pub static mut e: *mut libc::c_longlong = 0 as *const libc::c_longlong
    as *mut libc::c_longlong;
#[no_mangle]
pub static mut le: *mut libc::c_longlong = 0 as *const libc::c_longlong
    as *mut libc::c_longlong;
#[no_mangle]
pub static mut id: *mut libc::c_longlong = 0 as *const libc::c_longlong
    as *mut libc::c_longlong;
#[no_mangle]
pub static mut sym: *mut libc::c_longlong = 0 as *const libc::c_longlong
    as *mut libc::c_longlong;
#[no_mangle]
pub static mut tk: libc::c_longlong = 0;
#[no_mangle]
pub static mut ival: libc::c_longlong = 0;
#[no_mangle]
pub static mut ty: libc::c_longlong = 0;
#[no_mangle]
pub static mut loc: libc::c_longlong = 0;
#[no_mangle]
pub static mut line: libc::c_longlong = 0;
#[no_mangle]
pub static mut src: libc::c_longlong = 0;
#[no_mangle]
pub static mut debug: libc::c_longlong = 0;
#[no_mangle]
pub unsafe extern "C" fn next() {
    let mut pp: *mut libc::c_char = 0 as *mut libc::c_char;
    loop {
        tk = *p as libc::c_longlong;
        if !(tk != 0) {
            break;
        }
        p = p.offset(1);
        p;
        if tk == '\n' as i32 as libc::c_longlong {
            if src != 0 {
                printf(
                    b"%d: %.*s\0" as *const u8 as *const libc::c_char,
                    line,
                    p.offset_from(lp) as libc::c_long,
                    lp,
                );
                lp = p;
                while le < e {
                    le = le.offset(1);
                    printf(
                        b"%8.4s\0" as *const u8 as *const libc::c_char,
                        &*(b"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,\0"
                            as *const u8 as *const libc::c_char)
                            .offset(
                                (*le * 5 as libc::c_int as libc::c_longlong) as isize,
                            ) as *const libc::c_char,
                    );
                    if *le <= ADJ as libc::c_int as libc::c_longlong {
                        le = le.offset(1);
                        printf(b" %d\n\0" as *const u8 as *const libc::c_char, *le);
                    } else {
                        printf(b"\n\0" as *const u8 as *const libc::c_char);
                    }
                }
            }
            line += 1;
            line;
        } else if tk == '#' as i32 as libc::c_longlong {
            while *p as libc::c_int != 0 as libc::c_int
                && *p as libc::c_int != '\n' as i32
            {
                p = p.offset(1);
                p;
            }
        } else if tk >= 'a' as i32 as libc::c_longlong
            && tk <= 'z' as i32 as libc::c_longlong
            || tk >= 'A' as i32 as libc::c_longlong
                && tk <= 'Z' as i32 as libc::c_longlong
            || tk == '_' as i32 as libc::c_longlong
        {
            pp = p.offset(-(1 as libc::c_int as isize));
            while *p as libc::c_int >= 'a' as i32 && *p as libc::c_int <= 'z' as i32
                || *p as libc::c_int >= 'A' as i32 && *p as libc::c_int <= 'Z' as i32
                || *p as libc::c_int >= '0' as i32 && *p as libc::c_int <= '9' as i32
                || *p as libc::c_int == '_' as i32
            {
                let fresh0 = p;
                p = p.offset(1);
                tk = tk * 147 as libc::c_int as libc::c_longlong
                    + *fresh0 as libc::c_longlong;
            }
            tk = (tk << 6 as libc::c_int)
                + p.offset_from(pp) as libc::c_long as libc::c_longlong;
            id = sym;
            while *id.offset(Tk as libc::c_int as isize) != 0 {
                if tk == *id.offset(Hash as libc::c_int as isize)
                    && memcmp(
                        *id.offset(Name as libc::c_int as isize) as *mut libc::c_char
                            as *const libc::c_void,
                        pp as *const libc::c_void,
                        p.offset_from(pp) as libc::c_long as libc::c_ulong,
                    ) == 0
                {
                    tk = *id.offset(Tk as libc::c_int as isize);
                    return;
                }
                id = id.offset(Idsz as libc::c_int as isize);
            }
            *id.offset(Name as libc::c_int as isize) = pp as libc::c_longlong;
            *id.offset(Hash as libc::c_int as isize) = tk;
            let ref mut fresh1 = *id.offset(Tk as libc::c_int as isize);
            *fresh1 = Id as libc::c_int as libc::c_longlong;
            tk = *fresh1;
            return;
        } else if tk >= '0' as i32 as libc::c_longlong
            && tk <= '9' as i32 as libc::c_longlong
        {
            ival = tk - '0' as i32 as libc::c_longlong;
            if ival != 0 {
                while *p as libc::c_int >= '0' as i32 && *p as libc::c_int <= '9' as i32
                {
                    let fresh2 = p;
                    p = p.offset(1);
                    ival = ival * 10 as libc::c_int as libc::c_longlong
                        + *fresh2 as libc::c_longlong - '0' as i32 as libc::c_longlong;
                }
            } else if *p as libc::c_int == 'x' as i32 || *p as libc::c_int == 'X' as i32
            {
                loop {
                    p = p.offset(1);
                    tk = *p as libc::c_longlong;
                    if !(tk != 0
                        && (tk >= '0' as i32 as libc::c_longlong
                            && tk <= '9' as i32 as libc::c_longlong
                            || tk >= 'a' as i32 as libc::c_longlong
                                && tk <= 'f' as i32 as libc::c_longlong
                            || tk >= 'A' as i32 as libc::c_longlong
                                && tk <= 'F' as i32 as libc::c_longlong))
                    {
                        break;
                    }
                    ival = ival * 16 as libc::c_int as libc::c_longlong
                        + (tk & 15 as libc::c_int as libc::c_longlong)
                        + (if tk >= 'A' as i32 as libc::c_longlong {
                            9 as libc::c_int
                        } else {
                            0 as libc::c_int
                        }) as libc::c_longlong;
                }
            } else {
                while *p as libc::c_int >= '0' as i32 && *p as libc::c_int <= '7' as i32
                {
                    let fresh3 = p;
                    p = p.offset(1);
                    ival = ival * 8 as libc::c_int as libc::c_longlong
                        + *fresh3 as libc::c_longlong - '0' as i32 as libc::c_longlong;
                }
            }
            tk = Num as libc::c_int as libc::c_longlong;
            return;
        } else if tk == '/' as i32 as libc::c_longlong {
            if *p as libc::c_int == '/' as i32 {
                p = p.offset(1);
                p;
                while *p as libc::c_int != 0 as libc::c_int
                    && *p as libc::c_int != '\n' as i32
                {
                    p = p.offset(1);
                    p;
                }
            } else {
                tk = Div as libc::c_int as libc::c_longlong;
                return;
            }
        } else if tk == '\'' as i32 as libc::c_longlong
            || tk == '"' as i32 as libc::c_longlong
        {
            pp = data;
            while *p as libc::c_int != 0 as libc::c_int && *p as libc::c_longlong != tk {
                let fresh4 = p;
                p = p.offset(1);
                ival = *fresh4 as libc::c_longlong;
                if ival == '\\' as i32 as libc::c_longlong {
                    let fresh5 = p;
                    p = p.offset(1);
                    ival = *fresh5 as libc::c_longlong;
                    if ival == 'n' as i32 as libc::c_longlong {
                        ival = '\n' as i32 as libc::c_longlong;
                    }
                }
                if tk == '"' as i32 as libc::c_longlong {
                    let fresh6 = data;
                    data = data.offset(1);
                    *fresh6 = ival as libc::c_char;
                }
            }
            p = p.offset(1);
            p;
            if tk == '"' as i32 as libc::c_longlong {
                ival = pp as libc::c_longlong;
            } else {
                tk = Num as libc::c_int as libc::c_longlong;
            }
            return;
        } else if tk == '=' as i32 as libc::c_longlong {
            if *p as libc::c_int == '=' as i32 {
                p = p.offset(1);
                p;
                tk = Eq as libc::c_int as libc::c_longlong;
            } else {
                tk = Assign as libc::c_int as libc::c_longlong;
            }
            return;
        } else if tk == '+' as i32 as libc::c_longlong {
            if *p as libc::c_int == '+' as i32 {
                p = p.offset(1);
                p;
                tk = Inc as libc::c_int as libc::c_longlong;
            } else {
                tk = Add as libc::c_int as libc::c_longlong;
            }
            return;
        } else if tk == '-' as i32 as libc::c_longlong {
            if *p as libc::c_int == '-' as i32 {
                p = p.offset(1);
                p;
                tk = Dec as libc::c_int as libc::c_longlong;
            } else {
                tk = Sub as libc::c_int as libc::c_longlong;
            }
            return;
        } else if tk == '!' as i32 as libc::c_longlong {
            if *p as libc::c_int == '=' as i32 {
                p = p.offset(1);
                p;
                tk = Ne as libc::c_int as libc::c_longlong;
            }
            return;
        } else if tk == '<' as i32 as libc::c_longlong {
            if *p as libc::c_int == '=' as i32 {
                p = p.offset(1);
                p;
                tk = Le as libc::c_int as libc::c_longlong;
            } else if *p as libc::c_int == '<' as i32 {
                p = p.offset(1);
                p;
                tk = Shl as libc::c_int as libc::c_longlong;
            } else {
                tk = Lt as libc::c_int as libc::c_longlong;
            }
            return;
        } else if tk == '>' as i32 as libc::c_longlong {
            if *p as libc::c_int == '=' as i32 {
                p = p.offset(1);
                p;
                tk = Ge as libc::c_int as libc::c_longlong;
            } else if *p as libc::c_int == '>' as i32 {
                p = p.offset(1);
                p;
                tk = Shr as libc::c_int as libc::c_longlong;
            } else {
                tk = Gt as libc::c_int as libc::c_longlong;
            }
            return;
        } else if tk == '|' as i32 as libc::c_longlong {
            if *p as libc::c_int == '|' as i32 {
                p = p.offset(1);
                p;
                tk = Lor as libc::c_int as libc::c_longlong;
            } else {
                tk = Or as libc::c_int as libc::c_longlong;
            }
            return;
        } else if tk == '&' as i32 as libc::c_longlong {
            if *p as libc::c_int == '&' as i32 {
                p = p.offset(1);
                p;
                tk = Lan as libc::c_int as libc::c_longlong;
            } else {
                tk = And as libc::c_int as libc::c_longlong;
            }
            return;
        } else if tk == '^' as i32 as libc::c_longlong {
            tk = Xor as libc::c_int as libc::c_longlong;
            return;
        } else if tk == '%' as i32 as libc::c_longlong {
            tk = Mod as libc::c_int as libc::c_longlong;
            return;
        } else if tk == '*' as i32 as libc::c_longlong {
            tk = Mul as libc::c_int as libc::c_longlong;
            return;
        } else if tk == '[' as i32 as libc::c_longlong {
            tk = Brak as libc::c_int as libc::c_longlong;
            return;
        } else if tk == '?' as i32 as libc::c_longlong {
            tk = Cond as libc::c_int as libc::c_longlong;
            return;
        } else if tk == '~' as i32 as libc::c_longlong
            || tk == ';' as i32 as libc::c_longlong
            || tk == '{' as i32 as libc::c_longlong
            || tk == '}' as i32 as libc::c_longlong
            || tk == '(' as i32 as libc::c_longlong
            || tk == ')' as i32 as libc::c_longlong
            || tk == ']' as i32 as libc::c_longlong
            || tk == ',' as i32 as libc::c_longlong
            || tk == ':' as i32 as libc::c_longlong
        {
            return
        }
    };
}
#[no_mangle]
pub unsafe extern "C" fn expr(mut lev: libc::c_longlong) {
    let mut t: libc::c_longlong = 0;
    let mut d: *mut libc::c_longlong = 0 as *mut libc::c_longlong;
    if tk == 0 {
        printf(
            b"%d: unexpected eof in expression\n\0" as *const u8 as *const libc::c_char,
            line,
        );
        exit(-(1 as libc::c_int));
    } else if tk == Num as libc::c_int as libc::c_longlong {
        e = e.offset(1);
        *e = IMM as libc::c_int as libc::c_longlong;
        e = e.offset(1);
        *e = ival;
        next();
        ty = INT as libc::c_int as libc::c_longlong;
    } else if tk == '"' as i32 as libc::c_longlong {
        e = e.offset(1);
        *e = IMM as libc::c_int as libc::c_longlong;
        e = e.offset(1);
        *e = ival;
        next();
        while tk == '"' as i32 as libc::c_longlong {
            next();
        }
        data = ((data as libc::c_longlong as libc::c_ulonglong)
            .wrapping_add(
                ::core::mem::size_of::<libc::c_longlong>() as libc::c_ulong
                    as libc::c_ulonglong,
            )
            & (::core::mem::size_of::<libc::c_longlong>() as libc::c_ulong)
                .wrapping_neg() as libc::c_ulonglong) as *mut libc::c_char;
        ty = PTR as libc::c_int as libc::c_longlong;
    } else if tk == Sizeof as libc::c_int as libc::c_longlong {
        next();
        if tk == '(' as i32 as libc::c_longlong {
            next();
        } else {
            printf(
                b"%d: open paren expected in sizeof\n\0" as *const u8
                    as *const libc::c_char,
                line,
            );
            exit(-(1 as libc::c_int));
        }
        ty = INT as libc::c_int as libc::c_longlong;
        if tk == Int as libc::c_int as libc::c_longlong {
            next();
        } else if tk == Char as libc::c_int as libc::c_longlong {
            next();
            ty = CHAR as libc::c_int as libc::c_longlong;
        }
        while tk == Mul as libc::c_int as libc::c_longlong {
            next();
            ty = ty + PTR as libc::c_int as libc::c_longlong;
        }
        if tk == ')' as i32 as libc::c_longlong {
            next();
        } else {
            printf(
                b"%d: close paren expected in sizeof\n\0" as *const u8
                    as *const libc::c_char,
                line,
            );
            exit(-(1 as libc::c_int));
        }
        e = e.offset(1);
        *e = IMM as libc::c_int as libc::c_longlong;
        e = e.offset(1);
        *e = (if ty == CHAR as libc::c_int as libc::c_longlong {
            ::core::mem::size_of::<libc::c_char>() as libc::c_ulong
        } else {
            ::core::mem::size_of::<libc::c_longlong>() as libc::c_ulong
        }) as libc::c_longlong;
        ty = INT as libc::c_int as libc::c_longlong;
    } else if tk == Id as libc::c_int as libc::c_longlong {
        d = id;
        next();
        if tk == '(' as i32 as libc::c_longlong {
            next();
            t = 0 as libc::c_int as libc::c_longlong;
            while tk != ')' as i32 as libc::c_longlong {
                expr(Assign as libc::c_int as libc::c_longlong);
                e = e.offset(1);
                *e = PSH as libc::c_int as libc::c_longlong;
                t += 1;
                t;
                if tk == ',' as i32 as libc::c_longlong {
                    next();
                }
            }
            next();
            if *d.offset(Class as libc::c_int as isize)
                == Sys as libc::c_int as libc::c_longlong
            {
                e = e.offset(1);
                *e = *d.offset(Val as libc::c_int as isize);
            } else if *d.offset(Class as libc::c_int as isize)
                == Fun as libc::c_int as libc::c_longlong
            {
                e = e.offset(1);
                *e = JSR as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = *d.offset(Val as libc::c_int as isize);
            } else {
                printf(
                    b"%d: bad function call\n\0" as *const u8 as *const libc::c_char,
                    line,
                );
                exit(-(1 as libc::c_int));
            }
            if t != 0 {
                e = e.offset(1);
                *e = ADJ as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = t;
            }
            ty = *d.offset(Type as libc::c_int as isize);
        } else if *d.offset(Class as libc::c_int as isize)
            == Num as libc::c_int as libc::c_longlong
        {
            e = e.offset(1);
            *e = IMM as libc::c_int as libc::c_longlong;
            e = e.offset(1);
            *e = *d.offset(Val as libc::c_int as isize);
            ty = INT as libc::c_int as libc::c_longlong;
        } else {
            if *d.offset(Class as libc::c_int as isize)
                == Loc as libc::c_int as libc::c_longlong
            {
                e = e.offset(1);
                *e = LEA as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = loc - *d.offset(Val as libc::c_int as isize);
            } else if *d.offset(Class as libc::c_int as isize)
                == Glo as libc::c_int as libc::c_longlong
            {
                e = e.offset(1);
                *e = IMM as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = *d.offset(Val as libc::c_int as isize);
            } else {
                printf(
                    b"%d: undefined variable\n\0" as *const u8 as *const libc::c_char,
                    line,
                );
                exit(-(1 as libc::c_int));
            }
            ty = *d.offset(Type as libc::c_int as isize);
            e = e.offset(1);
            *e = (if ty == CHAR as libc::c_int as libc::c_longlong {
                LC as libc::c_int
            } else {
                LI as libc::c_int
            }) as libc::c_longlong;
        }
    } else if tk == '(' as i32 as libc::c_longlong {
        next();
        if tk == Int as libc::c_int as libc::c_longlong
            || tk == Char as libc::c_int as libc::c_longlong
        {
            t = (if tk == Int as libc::c_int as libc::c_longlong {
                INT as libc::c_int
            } else {
                CHAR as libc::c_int
            }) as libc::c_longlong;
            next();
            while tk == Mul as libc::c_int as libc::c_longlong {
                next();
                t = t + PTR as libc::c_int as libc::c_longlong;
            }
            if tk == ')' as i32 as libc::c_longlong {
                next();
            } else {
                printf(b"%d: bad cast\n\0" as *const u8 as *const libc::c_char, line);
                exit(-(1 as libc::c_int));
            }
            expr(Inc as libc::c_int as libc::c_longlong);
            ty = t;
        } else {
            expr(Assign as libc::c_int as libc::c_longlong);
            if tk == ')' as i32 as libc::c_longlong {
                next();
            } else {
                printf(
                    b"%d: close paren expected\n\0" as *const u8 as *const libc::c_char,
                    line,
                );
                exit(-(1 as libc::c_int));
            }
        }
    } else if tk == Mul as libc::c_int as libc::c_longlong {
        next();
        expr(Inc as libc::c_int as libc::c_longlong);
        if ty > INT as libc::c_int as libc::c_longlong {
            ty = ty - PTR as libc::c_int as libc::c_longlong;
        } else {
            printf(b"%d: bad dereference\n\0" as *const u8 as *const libc::c_char, line);
            exit(-(1 as libc::c_int));
        }
        e = e.offset(1);
        *e = (if ty == CHAR as libc::c_int as libc::c_longlong {
            LC as libc::c_int
        } else {
            LI as libc::c_int
        }) as libc::c_longlong;
    } else if tk == And as libc::c_int as libc::c_longlong {
        next();
        expr(Inc as libc::c_int as libc::c_longlong);
        if *e == LC as libc::c_int as libc::c_longlong
            || *e == LI as libc::c_int as libc::c_longlong
        {
            e = e.offset(-1);
            e;
        } else {
            printf(b"%d: bad address-of\n\0" as *const u8 as *const libc::c_char, line);
            exit(-(1 as libc::c_int));
        }
        ty = ty + PTR as libc::c_int as libc::c_longlong;
    } else if tk == '!' as i32 as libc::c_longlong {
        next();
        expr(Inc as libc::c_int as libc::c_longlong);
        e = e.offset(1);
        *e = PSH as libc::c_int as libc::c_longlong;
        e = e.offset(1);
        *e = IMM as libc::c_int as libc::c_longlong;
        e = e.offset(1);
        *e = 0 as libc::c_int as libc::c_longlong;
        e = e.offset(1);
        *e = EQ as libc::c_int as libc::c_longlong;
        ty = INT as libc::c_int as libc::c_longlong;
    } else if tk == '~' as i32 as libc::c_longlong {
        next();
        expr(Inc as libc::c_int as libc::c_longlong);
        e = e.offset(1);
        *e = PSH as libc::c_int as libc::c_longlong;
        e = e.offset(1);
        *e = IMM as libc::c_int as libc::c_longlong;
        e = e.offset(1);
        *e = -(1 as libc::c_int) as libc::c_longlong;
        e = e.offset(1);
        *e = XOR as libc::c_int as libc::c_longlong;
        ty = INT as libc::c_int as libc::c_longlong;
    } else if tk == Add as libc::c_int as libc::c_longlong {
        next();
        expr(Inc as libc::c_int as libc::c_longlong);
        ty = INT as libc::c_int as libc::c_longlong;
    } else if tk == Sub as libc::c_int as libc::c_longlong {
        next();
        e = e.offset(1);
        *e = IMM as libc::c_int as libc::c_longlong;
        if tk == Num as libc::c_int as libc::c_longlong {
            e = e.offset(1);
            *e = -ival;
            next();
        } else {
            e = e.offset(1);
            *e = -(1 as libc::c_int) as libc::c_longlong;
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Inc as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = MUL as libc::c_int as libc::c_longlong;
        }
        ty = INT as libc::c_int as libc::c_longlong;
    } else if tk == Inc as libc::c_int as libc::c_longlong
        || tk == Dec as libc::c_int as libc::c_longlong
    {
        t = tk;
        next();
        expr(Inc as libc::c_int as libc::c_longlong);
        if *e == LC as libc::c_int as libc::c_longlong {
            *e = PSH as libc::c_int as libc::c_longlong;
            e = e.offset(1);
            *e = LC as libc::c_int as libc::c_longlong;
        } else if *e == LI as libc::c_int as libc::c_longlong {
            *e = PSH as libc::c_int as libc::c_longlong;
            e = e.offset(1);
            *e = LI as libc::c_int as libc::c_longlong;
        } else {
            printf(
                b"%d: bad lvalue in pre-increment\n\0" as *const u8
                    as *const libc::c_char,
                line,
            );
            exit(-(1 as libc::c_int));
        }
        e = e.offset(1);
        *e = PSH as libc::c_int as libc::c_longlong;
        e = e.offset(1);
        *e = IMM as libc::c_int as libc::c_longlong;
        e = e.offset(1);
        *e = (if ty > PTR as libc::c_int as libc::c_longlong {
            ::core::mem::size_of::<libc::c_longlong>() as libc::c_ulong
        } else {
            ::core::mem::size_of::<libc::c_char>() as libc::c_ulong
        }) as libc::c_longlong;
        e = e.offset(1);
        *e = (if t == Inc as libc::c_int as libc::c_longlong {
            ADD as libc::c_int
        } else {
            SUB as libc::c_int
        }) as libc::c_longlong;
        e = e.offset(1);
        *e = (if ty == CHAR as libc::c_int as libc::c_longlong {
            SC as libc::c_int
        } else {
            SI as libc::c_int
        }) as libc::c_longlong;
    } else {
        printf(b"%d: bad expression\n\0" as *const u8 as *const libc::c_char, line);
        exit(-(1 as libc::c_int));
    }
    while tk >= lev {
        t = ty;
        if tk == Assign as libc::c_int as libc::c_longlong {
            next();
            if *e == LC as libc::c_int as libc::c_longlong
                || *e == LI as libc::c_int as libc::c_longlong
            {
                *e = PSH as libc::c_int as libc::c_longlong;
            } else {
                printf(
                    b"%d: bad lvalue in assignment\n\0" as *const u8
                        as *const libc::c_char,
                    line,
                );
                exit(-(1 as libc::c_int));
            }
            expr(Assign as libc::c_int as libc::c_longlong);
            ty = t;
            e = e.offset(1);
            *e = (if ty == CHAR as libc::c_int as libc::c_longlong {
                SC as libc::c_int
            } else {
                SI as libc::c_int
            }) as libc::c_longlong;
        } else if tk == Cond as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = BZ as libc::c_int as libc::c_longlong;
            e = e.offset(1);
            d = e;
            expr(Assign as libc::c_int as libc::c_longlong);
            if tk == ':' as i32 as libc::c_longlong {
                next();
            } else {
                printf(
                    b"%d: conditional missing colon\n\0" as *const u8
                        as *const libc::c_char,
                    line,
                );
                exit(-(1 as libc::c_int));
            }
            *d = e.offset(3 as libc::c_int as isize) as libc::c_longlong;
            e = e.offset(1);
            *e = JMP as libc::c_int as libc::c_longlong;
            e = e.offset(1);
            d = e;
            expr(Cond as libc::c_int as libc::c_longlong);
            *d = e.offset(1 as libc::c_int as isize) as libc::c_longlong;
        } else if tk == Lor as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = BNZ as libc::c_int as libc::c_longlong;
            e = e.offset(1);
            d = e;
            expr(Lan as libc::c_int as libc::c_longlong);
            *d = e.offset(1 as libc::c_int as isize) as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Lan as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = BZ as libc::c_int as libc::c_longlong;
            e = e.offset(1);
            d = e;
            expr(Or as libc::c_int as libc::c_longlong);
            *d = e.offset(1 as libc::c_int as isize) as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Or as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Xor as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = OR as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Xor as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(And as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = XOR as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == And as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Eq as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = AND as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Eq as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Lt as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = EQ as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Ne as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Lt as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = NE as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Lt as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Shl as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = LT as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Gt as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Shl as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = GT as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Le as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Shl as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = LE as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Ge as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Shl as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = GE as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Shl as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Add as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = SHL as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Shr as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Add as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = SHR as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Add as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Mul as libc::c_int as libc::c_longlong);
            ty = t;
            if ty > PTR as libc::c_int as libc::c_longlong {
                e = e.offset(1);
                *e = PSH as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = IMM as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = ::core::mem::size_of::<libc::c_longlong>() as libc::c_ulong
                    as libc::c_longlong;
                e = e.offset(1);
                *e = MUL as libc::c_int as libc::c_longlong;
            }
            e = e.offset(1);
            *e = ADD as libc::c_int as libc::c_longlong;
        } else if tk == Sub as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Mul as libc::c_int as libc::c_longlong);
            if t > PTR as libc::c_int as libc::c_longlong && t == ty {
                e = e.offset(1);
                *e = SUB as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = PSH as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = IMM as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = ::core::mem::size_of::<libc::c_longlong>() as libc::c_ulong
                    as libc::c_longlong;
                e = e.offset(1);
                *e = DIV as libc::c_int as libc::c_longlong;
                ty = INT as libc::c_int as libc::c_longlong;
            } else {
                ty = t;
                if ty > PTR as libc::c_int as libc::c_longlong {
                    e = e.offset(1);
                    *e = PSH as libc::c_int as libc::c_longlong;
                    e = e.offset(1);
                    *e = IMM as libc::c_int as libc::c_longlong;
                    e = e.offset(1);
                    *e = ::core::mem::size_of::<libc::c_longlong>() as libc::c_ulong
                        as libc::c_longlong;
                    e = e.offset(1);
                    *e = MUL as libc::c_int as libc::c_longlong;
                    e = e.offset(1);
                    *e = SUB as libc::c_int as libc::c_longlong;
                } else {
                    e = e.offset(1);
                    *e = SUB as libc::c_int as libc::c_longlong;
                }
            }
        } else if tk == Mul as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Inc as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = MUL as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Div as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Inc as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = DIV as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Mod as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Inc as libc::c_int as libc::c_longlong);
            e = e.offset(1);
            *e = MOD as libc::c_int as libc::c_longlong;
            ty = INT as libc::c_int as libc::c_longlong;
        } else if tk == Inc as libc::c_int as libc::c_longlong
            || tk == Dec as libc::c_int as libc::c_longlong
        {
            if *e == LC as libc::c_int as libc::c_longlong {
                *e = PSH as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = LC as libc::c_int as libc::c_longlong;
            } else if *e == LI as libc::c_int as libc::c_longlong {
                *e = PSH as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = LI as libc::c_int as libc::c_longlong;
            } else {
                printf(
                    b"%d: bad lvalue in post-increment\n\0" as *const u8
                        as *const libc::c_char,
                    line,
                );
                exit(-(1 as libc::c_int));
            }
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            e = e.offset(1);
            *e = IMM as libc::c_int as libc::c_longlong;
            e = e.offset(1);
            *e = (if ty > PTR as libc::c_int as libc::c_longlong {
                ::core::mem::size_of::<libc::c_longlong>() as libc::c_ulong
            } else {
                ::core::mem::size_of::<libc::c_char>() as libc::c_ulong
            }) as libc::c_longlong;
            e = e.offset(1);
            *e = (if tk == Inc as libc::c_int as libc::c_longlong {
                ADD as libc::c_int
            } else {
                SUB as libc::c_int
            }) as libc::c_longlong;
            e = e.offset(1);
            *e = (if ty == CHAR as libc::c_int as libc::c_longlong {
                SC as libc::c_int
            } else {
                SI as libc::c_int
            }) as libc::c_longlong;
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            e = e.offset(1);
            *e = IMM as libc::c_int as libc::c_longlong;
            e = e.offset(1);
            *e = (if ty > PTR as libc::c_int as libc::c_longlong {
                ::core::mem::size_of::<libc::c_longlong>() as libc::c_ulong
            } else {
                ::core::mem::size_of::<libc::c_char>() as libc::c_ulong
            }) as libc::c_longlong;
            e = e.offset(1);
            *e = (if tk == Inc as libc::c_int as libc::c_longlong {
                SUB as libc::c_int
            } else {
                ADD as libc::c_int
            }) as libc::c_longlong;
            next();
        } else if tk == Brak as libc::c_int as libc::c_longlong {
            next();
            e = e.offset(1);
            *e = PSH as libc::c_int as libc::c_longlong;
            expr(Assign as libc::c_int as libc::c_longlong);
            if tk == ']' as i32 as libc::c_longlong {
                next();
            } else {
                printf(
                    b"%d: close bracket expected\n\0" as *const u8
                        as *const libc::c_char,
                    line,
                );
                exit(-(1 as libc::c_int));
            }
            if t > PTR as libc::c_int as libc::c_longlong {
                e = e.offset(1);
                *e = PSH as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = IMM as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = ::core::mem::size_of::<libc::c_longlong>() as libc::c_ulong
                    as libc::c_longlong;
                e = e.offset(1);
                *e = MUL as libc::c_int as libc::c_longlong;
            } else if t < PTR as libc::c_int as libc::c_longlong {
                printf(
                    b"%d: pointer type expected\n\0" as *const u8 as *const libc::c_char,
                    line,
                );
                exit(-(1 as libc::c_int));
            }
            e = e.offset(1);
            *e = ADD as libc::c_int as libc::c_longlong;
            ty = t - PTR as libc::c_int as libc::c_longlong;
            e = e.offset(1);
            *e = (if ty == CHAR as libc::c_int as libc::c_longlong {
                LC as libc::c_int
            } else {
                LI as libc::c_int
            }) as libc::c_longlong;
        } else {
            printf(
                b"%d: compiler error tk=%d\n\0" as *const u8 as *const libc::c_char,
                line,
                tk,
            );
            exit(-(1 as libc::c_int));
        }
    }
}
#[no_mangle]
pub unsafe extern "C" fn stmt() {
    let mut a: *mut libc::c_longlong = 0 as *mut libc::c_longlong;
    let mut b: *mut libc::c_longlong = 0 as *mut libc::c_longlong;
    if tk == If as libc::c_int as libc::c_longlong {
        next();
        if tk == '(' as i32 as libc::c_longlong {
            next();
        } else {
            printf(
                b"%d: open paren expected\n\0" as *const u8 as *const libc::c_char,
                line,
            );
            exit(-(1 as libc::c_int));
        }
        expr(Assign as libc::c_int as libc::c_longlong);
        if tk == ')' as i32 as libc::c_longlong {
            next();
        } else {
            printf(
                b"%d: close paren expected\n\0" as *const u8 as *const libc::c_char,
                line,
            );
            exit(-(1 as libc::c_int));
        }
        e = e.offset(1);
        *e = BZ as libc::c_int as libc::c_longlong;
        e = e.offset(1);
        b = e;
        stmt();
        if tk == Else as libc::c_int as libc::c_longlong {
            *b = e.offset(3 as libc::c_int as isize) as libc::c_longlong;
            e = e.offset(1);
            *e = JMP as libc::c_int as libc::c_longlong;
            e = e.offset(1);
            b = e;
            next();
            stmt();
        }
        *b = e.offset(1 as libc::c_int as isize) as libc::c_longlong;
    } else if tk == While as libc::c_int as libc::c_longlong {
        next();
        a = e.offset(1 as libc::c_int as isize);
        if tk == '(' as i32 as libc::c_longlong {
            next();
        } else {
            printf(
                b"%d: open paren expected\n\0" as *const u8 as *const libc::c_char,
                line,
            );
            exit(-(1 as libc::c_int));
        }
        expr(Assign as libc::c_int as libc::c_longlong);
        if tk == ')' as i32 as libc::c_longlong {
            next();
        } else {
            printf(
                b"%d: close paren expected\n\0" as *const u8 as *const libc::c_char,
                line,
            );
            exit(-(1 as libc::c_int));
        }
        e = e.offset(1);
        *e = BZ as libc::c_int as libc::c_longlong;
        e = e.offset(1);
        b = e;
        stmt();
        e = e.offset(1);
        *e = JMP as libc::c_int as libc::c_longlong;
        e = e.offset(1);
        *e = a as libc::c_longlong;
        *b = e.offset(1 as libc::c_int as isize) as libc::c_longlong;
    } else if tk == Return as libc::c_int as libc::c_longlong {
        next();
        if tk != ';' as i32 as libc::c_longlong {
            expr(Assign as libc::c_int as libc::c_longlong);
        }
        e = e.offset(1);
        *e = LEV as libc::c_int as libc::c_longlong;
        if tk == ';' as i32 as libc::c_longlong {
            next();
        } else {
            printf(
                b"%d: semicolon expected\n\0" as *const u8 as *const libc::c_char,
                line,
            );
            exit(-(1 as libc::c_int));
        }
    } else if tk == '{' as i32 as libc::c_longlong {
        next();
        while tk != '}' as i32 as libc::c_longlong {
            stmt();
        }
        next();
    } else if tk == ';' as i32 as libc::c_longlong {
        next();
    } else {
        expr(Assign as libc::c_int as libc::c_longlong);
        if tk == ';' as i32 as libc::c_longlong {
            next();
        } else {
            printf(
                b"%d: semicolon expected\n\0" as *const u8 as *const libc::c_char,
                line,
            );
            exit(-(1 as libc::c_int));
        }
    };
}
unsafe fn main_0(
    mut argc: libc::c_longlong,
    mut argv: *mut *mut libc::c_char,
) -> libc::c_longlong {
    let mut fd: libc::c_longlong = 0;
    let mut bt: libc::c_longlong = 0;
    let mut ty_0: libc::c_longlong = 0;
    let mut poolsz: libc::c_longlong = 0;
    let mut idmain: *mut libc::c_longlong = 0 as *mut libc::c_longlong;
    let mut pc: *mut libc::c_longlong = 0 as *mut libc::c_longlong;
    let mut sp: *mut libc::c_longlong = 0 as *mut libc::c_longlong;
    let mut bp: *mut libc::c_longlong = 0 as *mut libc::c_longlong;
    let mut a: libc::c_longlong = 0;
    let mut cycle: libc::c_longlong = 0;
    let mut i: libc::c_longlong = 0;
    let mut t: *mut libc::c_longlong = 0 as *mut libc::c_longlong;
    argc -= 1;
    argc;
    argv = argv.offset(1);
    argv;
    if argc > 0 as libc::c_int as libc::c_longlong && **argv as libc::c_int == '-' as i32
        && *(*argv).offset(1 as libc::c_int as isize) as libc::c_int == 's' as i32
    {
        src = 1 as libc::c_int as libc::c_longlong;
        argc -= 1;
        argc;
        argv = argv.offset(1);
        argv;
    }
    if argc > 0 as libc::c_int as libc::c_longlong && **argv as libc::c_int == '-' as i32
        && *(*argv).offset(1 as libc::c_int as isize) as libc::c_int == 'd' as i32
    {
        debug = 1 as libc::c_int as libc::c_longlong;
        argc -= 1;
        argc;
        argv = argv.offset(1);
        argv;
    }
    if argc < 1 as libc::c_int as libc::c_longlong {
        printf(b"usage: c4 [-s] [-d] file ...\n\0" as *const u8 as *const libc::c_char);
        return -(1 as libc::c_int) as libc::c_longlong;
    }
    fd = open(*argv, 0 as libc::c_int) as libc::c_longlong;
    if fd < 0 as libc::c_int as libc::c_longlong {
        printf(b"could not open(%s)\n\0" as *const u8 as *const libc::c_char, *argv);
        return -(1 as libc::c_int) as libc::c_longlong;
    }
    poolsz = (256 as libc::c_int * 1024 as libc::c_int) as libc::c_longlong;
    sym = malloc(poolsz as libc::c_ulong) as *mut libc::c_longlong;
    if sym.is_null() {
        printf(
            b"could not malloc(%d) symbol area\n\0" as *const u8 as *const libc::c_char,
            poolsz,
        );
        return -(1 as libc::c_int) as libc::c_longlong;
    }
    e = malloc(poolsz as libc::c_ulong) as *mut libc::c_longlong;
    le = e;
    if le.is_null() {
        printf(
            b"could not malloc(%d) text area\n\0" as *const u8 as *const libc::c_char,
            poolsz,
        );
        return -(1 as libc::c_int) as libc::c_longlong;
    }
    data = malloc(poolsz as libc::c_ulong) as *mut libc::c_char;
    if data.is_null() {
        printf(
            b"could not malloc(%d) data area\n\0" as *const u8 as *const libc::c_char,
            poolsz,
        );
        return -(1 as libc::c_int) as libc::c_longlong;
    }
    sp = malloc(poolsz as libc::c_ulong) as *mut libc::c_longlong;
    if sp.is_null() {
        printf(
            b"could not malloc(%d) stack area\n\0" as *const u8 as *const libc::c_char,
            poolsz,
        );
        return -(1 as libc::c_int) as libc::c_longlong;
    }
    memset(sym as *mut libc::c_void, 0 as libc::c_int, poolsz as libc::c_ulong);
    memset(e as *mut libc::c_void, 0 as libc::c_int, poolsz as libc::c_ulong);
    memset(data as *mut libc::c_void, 0 as libc::c_int, poolsz as libc::c_ulong);
    p = b"char else enum if int return sizeof while open read close printf malloc free memset memcmp exit void main\0"
        as *const u8 as *const libc::c_char as *mut libc::c_char;
    i = Char as libc::c_int as libc::c_longlong;
    while i <= While as libc::c_int as libc::c_longlong {
        next();
        let fresh7 = i;
        i = i + 1;
        *id.offset(Tk as libc::c_int as isize) = fresh7;
    }
    i = OPEN as libc::c_int as libc::c_longlong;
    while i <= EXIT as libc::c_int as libc::c_longlong {
        next();
        *id
            .offset(
                Class as libc::c_int as isize,
            ) = Sys as libc::c_int as libc::c_longlong;
        *id
            .offset(
                Type as libc::c_int as isize,
            ) = INT as libc::c_int as libc::c_longlong;
        let fresh8 = i;
        i = i + 1;
        *id.offset(Val as libc::c_int as isize) = fresh8;
    }
    next();
    *id.offset(Tk as libc::c_int as isize) = Char as libc::c_int as libc::c_longlong;
    next();
    idmain = id;
    p = malloc(poolsz as libc::c_ulong) as *mut libc::c_char;
    lp = p;
    if lp.is_null() {
        printf(
            b"could not malloc(%d) source area\n\0" as *const u8 as *const libc::c_char,
            poolsz,
        );
        return -(1 as libc::c_int) as libc::c_longlong;
    }
    i = read(
        fd as libc::c_int,
        p as *mut libc::c_void,
        (poolsz - 1 as libc::c_int as libc::c_longlong) as size_t,
    ) as libc::c_longlong;
    if i <= 0 as libc::c_int as libc::c_longlong {
        printf(b"read() returned %d\n\0" as *const u8 as *const libc::c_char, i);
        return -(1 as libc::c_int) as libc::c_longlong;
    }
    *p.offset(i as isize) = 0 as libc::c_int as libc::c_char;
    close(fd as libc::c_int);
    line = 1 as libc::c_int as libc::c_longlong;
    next();
    while tk != 0 {
        bt = INT as libc::c_int as libc::c_longlong;
        if tk == Int as libc::c_int as libc::c_longlong {
            next();
        } else if tk == Char as libc::c_int as libc::c_longlong {
            next();
            bt = CHAR as libc::c_int as libc::c_longlong;
        } else if tk == Enum as libc::c_int as libc::c_longlong {
            next();
            if tk != '{' as i32 as libc::c_longlong {
                next();
            }
            if tk == '{' as i32 as libc::c_longlong {
                next();
                i = 0 as libc::c_int as libc::c_longlong;
                while tk != '}' as i32 as libc::c_longlong {
                    if tk != Id as libc::c_int as libc::c_longlong {
                        printf(
                            b"%d: bad enum identifier %d\n\0" as *const u8
                                as *const libc::c_char,
                            line,
                            tk,
                        );
                        return -(1 as libc::c_int) as libc::c_longlong;
                    }
                    next();
                    if tk == Assign as libc::c_int as libc::c_longlong {
                        next();
                        if tk != Num as libc::c_int as libc::c_longlong {
                            printf(
                                b"%d: bad enum initializer\n\0" as *const u8
                                    as *const libc::c_char,
                                line,
                            );
                            return -(1 as libc::c_int) as libc::c_longlong;
                        }
                        i = ival;
                        next();
                    }
                    *id
                        .offset(
                            Class as libc::c_int as isize,
                        ) = Num as libc::c_int as libc::c_longlong;
                    *id
                        .offset(
                            Type as libc::c_int as isize,
                        ) = INT as libc::c_int as libc::c_longlong;
                    let fresh9 = i;
                    i = i + 1;
                    *id.offset(Val as libc::c_int as isize) = fresh9;
                    if tk == ',' as i32 as libc::c_longlong {
                        next();
                    }
                }
                next();
            }
        }
        while tk != ';' as i32 as libc::c_longlong
            && tk != '}' as i32 as libc::c_longlong
        {
            ty_0 = bt;
            while tk == Mul as libc::c_int as libc::c_longlong {
                next();
                ty_0 = ty_0 + PTR as libc::c_int as libc::c_longlong;
            }
            if tk != Id as libc::c_int as libc::c_longlong {
                printf(
                    b"%d: bad global declaration\n\0" as *const u8
                        as *const libc::c_char,
                    line,
                );
                return -(1 as libc::c_int) as libc::c_longlong;
            }
            if *id.offset(Class as libc::c_int as isize) != 0 {
                printf(
                    b"%d: duplicate global definition\n\0" as *const u8
                        as *const libc::c_char,
                    line,
                );
                return -(1 as libc::c_int) as libc::c_longlong;
            }
            next();
            *id.offset(Type as libc::c_int as isize) = ty_0;
            if tk == '(' as i32 as libc::c_longlong {
                *id
                    .offset(
                        Class as libc::c_int as isize,
                    ) = Fun as libc::c_int as libc::c_longlong;
                *id
                    .offset(
                        Val as libc::c_int as isize,
                    ) = e.offset(1 as libc::c_int as isize) as libc::c_longlong;
                next();
                i = 0 as libc::c_int as libc::c_longlong;
                while tk != ')' as i32 as libc::c_longlong {
                    ty_0 = INT as libc::c_int as libc::c_longlong;
                    if tk == Int as libc::c_int as libc::c_longlong {
                        next();
                    } else if tk == Char as libc::c_int as libc::c_longlong {
                        next();
                        ty_0 = CHAR as libc::c_int as libc::c_longlong;
                    }
                    while tk == Mul as libc::c_int as libc::c_longlong {
                        next();
                        ty_0 = ty_0 + PTR as libc::c_int as libc::c_longlong;
                    }
                    if tk != Id as libc::c_int as libc::c_longlong {
                        printf(
                            b"%d: bad parameter declaration\n\0" as *const u8
                                as *const libc::c_char,
                            line,
                        );
                        return -(1 as libc::c_int) as libc::c_longlong;
                    }
                    if *id.offset(Class as libc::c_int as isize)
                        == Loc as libc::c_int as libc::c_longlong
                    {
                        printf(
                            b"%d: duplicate parameter definition\n\0" as *const u8
                                as *const libc::c_char,
                            line,
                        );
                        return -(1 as libc::c_int) as libc::c_longlong;
                    }
                    *id
                        .offset(
                            HClass as libc::c_int as isize,
                        ) = *id.offset(Class as libc::c_int as isize);
                    *id
                        .offset(
                            Class as libc::c_int as isize,
                        ) = Loc as libc::c_int as libc::c_longlong;
                    *id
                        .offset(
                            HType as libc::c_int as isize,
                        ) = *id.offset(Type as libc::c_int as isize);
                    *id.offset(Type as libc::c_int as isize) = ty_0;
                    *id
                        .offset(
                            HVal as libc::c_int as isize,
                        ) = *id.offset(Val as libc::c_int as isize);
                    let fresh10 = i;
                    i = i + 1;
                    *id.offset(Val as libc::c_int as isize) = fresh10;
                    next();
                    if tk == ',' as i32 as libc::c_longlong {
                        next();
                    }
                }
                next();
                if tk != '{' as i32 as libc::c_longlong {
                    printf(
                        b"%d: bad function definition\n\0" as *const u8
                            as *const libc::c_char,
                        line,
                    );
                    return -(1 as libc::c_int) as libc::c_longlong;
                }
                i += 1;
                loc = i;
                next();
                while tk == Int as libc::c_int as libc::c_longlong
                    || tk == Char as libc::c_int as libc::c_longlong
                {
                    bt = (if tk == Int as libc::c_int as libc::c_longlong {
                        INT as libc::c_int
                    } else {
                        CHAR as libc::c_int
                    }) as libc::c_longlong;
                    next();
                    while tk != ';' as i32 as libc::c_longlong {
                        ty_0 = bt;
                        while tk == Mul as libc::c_int as libc::c_longlong {
                            next();
                            ty_0 = ty_0 + PTR as libc::c_int as libc::c_longlong;
                        }
                        if tk != Id as libc::c_int as libc::c_longlong {
                            printf(
                                b"%d: bad local declaration\n\0" as *const u8
                                    as *const libc::c_char,
                                line,
                            );
                            return -(1 as libc::c_int) as libc::c_longlong;
                        }
                        if *id.offset(Class as libc::c_int as isize)
                            == Loc as libc::c_int as libc::c_longlong
                        {
                            printf(
                                b"%d: duplicate local definition\n\0" as *const u8
                                    as *const libc::c_char,
                                line,
                            );
                            return -(1 as libc::c_int) as libc::c_longlong;
                        }
                        *id
                            .offset(
                                HClass as libc::c_int as isize,
                            ) = *id.offset(Class as libc::c_int as isize);
                        *id
                            .offset(
                                Class as libc::c_int as isize,
                            ) = Loc as libc::c_int as libc::c_longlong;
                        *id
                            .offset(
                                HType as libc::c_int as isize,
                            ) = *id.offset(Type as libc::c_int as isize);
                        *id.offset(Type as libc::c_int as isize) = ty_0;
                        *id
                            .offset(
                                HVal as libc::c_int as isize,
                            ) = *id.offset(Val as libc::c_int as isize);
                        i += 1;
                        *id.offset(Val as libc::c_int as isize) = i;
                        next();
                        if tk == ',' as i32 as libc::c_longlong {
                            next();
                        }
                    }
                    next();
                }
                e = e.offset(1);
                *e = ENT as libc::c_int as libc::c_longlong;
                e = e.offset(1);
                *e = i - loc;
                while tk != '}' as i32 as libc::c_longlong {
                    stmt();
                }
                e = e.offset(1);
                *e = LEV as libc::c_int as libc::c_longlong;
                id = sym;
                while *id.offset(Tk as libc::c_int as isize) != 0 {
                    if *id.offset(Class as libc::c_int as isize)
                        == Loc as libc::c_int as libc::c_longlong
                    {
                        *id
                            .offset(
                                Class as libc::c_int as isize,
                            ) = *id.offset(HClass as libc::c_int as isize);
                        *id
                            .offset(
                                Type as libc::c_int as isize,
                            ) = *id.offset(HType as libc::c_int as isize);
                        *id
                            .offset(
                                Val as libc::c_int as isize,
                            ) = *id.offset(HVal as libc::c_int as isize);
                    }
                    id = id.offset(Idsz as libc::c_int as isize);
                }
            } else {
                *id
                    .offset(
                        Class as libc::c_int as isize,
                    ) = Glo as libc::c_int as libc::c_longlong;
                *id.offset(Val as libc::c_int as isize) = data as libc::c_longlong;
                data = data
                    .offset(
                        ::core::mem::size_of::<libc::c_longlong>() as libc::c_ulong
                            as isize,
                    );
            }
            if tk == ',' as i32 as libc::c_longlong {
                next();
            }
        }
        next();
    }
    pc = *idmain.offset(Val as libc::c_int as isize) as *mut libc::c_longlong;
    if pc.is_null() {
        printf(b"main() not defined\n\0" as *const u8 as *const libc::c_char);
        return -(1 as libc::c_int) as libc::c_longlong;
    }
    if src != 0 {
        return 0 as libc::c_int as libc::c_longlong;
    }
    sp = (sp as libc::c_longlong + poolsz) as *mut libc::c_longlong;
    bp = sp;
    sp = sp.offset(-1);
    *sp = EXIT as libc::c_int as libc::c_longlong;
    sp = sp.offset(-1);
    *sp = PSH as libc::c_int as libc::c_longlong;
    t = sp;
    sp = sp.offset(-1);
    *sp = argc;
    sp = sp.offset(-1);
    *sp = argv as libc::c_longlong;
    sp = sp.offset(-1);
    *sp = t as libc::c_longlong;
    cycle = 0 as libc::c_int as libc::c_longlong;
    loop {
        let fresh11 = pc;
        pc = pc.offset(1);
        i = *fresh11;
        cycle += 1;
        cycle;
        if debug != 0 {
            printf(
                b"%d> %.4s\0" as *const u8 as *const libc::c_char,
                cycle,
                &*(b"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,\0"
                    as *const u8 as *const libc::c_char)
                    .offset((i * 5 as libc::c_int as libc::c_longlong) as isize)
                    as *const libc::c_char,
            );
            if i <= ADJ as libc::c_int as libc::c_longlong {
                printf(b" %d\n\0" as *const u8 as *const libc::c_char, *pc);
            } else {
                printf(b"\n\0" as *const u8 as *const libc::c_char);
            }
        }
        if i == LEA as libc::c_int as libc::c_longlong {
            let fresh12 = pc;
            pc = pc.offset(1);
            a = bp.offset(*fresh12 as isize) as libc::c_longlong;
        } else if i == IMM as libc::c_int as libc::c_longlong {
            let fresh13 = pc;
            pc = pc.offset(1);
            a = *fresh13;
        } else if i == JMP as libc::c_int as libc::c_longlong {
            pc = *pc as *mut libc::c_longlong;
        } else if i == JSR as libc::c_int as libc::c_longlong {
            sp = sp.offset(-1);
            *sp = pc.offset(1 as libc::c_int as isize) as libc::c_longlong;
            pc = *pc as *mut libc::c_longlong;
        } else if i == BZ as libc::c_int as libc::c_longlong {
            pc = if a != 0 {
                pc.offset(1 as libc::c_int as isize)
            } else {
                *pc as *mut libc::c_longlong
            };
        } else if i == BNZ as libc::c_int as libc::c_longlong {
            pc = if a != 0 {
                *pc as *mut libc::c_longlong
            } else {
                pc.offset(1 as libc::c_int as isize)
            };
        } else if i == ENT as libc::c_int as libc::c_longlong {
            sp = sp.offset(-1);
            *sp = bp as libc::c_longlong;
            bp = sp;
            let fresh14 = pc;
            pc = pc.offset(1);
            sp = sp.offset(-(*fresh14 as isize));
        } else if i == ADJ as libc::c_int as libc::c_longlong {
            let fresh15 = pc;
            pc = pc.offset(1);
            sp = sp.offset(*fresh15 as isize);
        } else if i == LEV as libc::c_int as libc::c_longlong {
            sp = bp;
            let fresh16 = sp;
            sp = sp.offset(1);
            bp = *fresh16 as *mut libc::c_longlong;
            let fresh17 = sp;
            sp = sp.offset(1);
            pc = *fresh17 as *mut libc::c_longlong;
        } else if i == LI as libc::c_int as libc::c_longlong {
            a = *(a as *mut libc::c_longlong);
        } else if i == LC as libc::c_int as libc::c_longlong {
            a = *(a as *mut libc::c_char) as libc::c_longlong;
        } else if i == SI as libc::c_int as libc::c_longlong {
            let fresh18 = sp;
            sp = sp.offset(1);
            *(*fresh18 as *mut libc::c_longlong) = a;
        } else if i == SC as libc::c_int as libc::c_longlong {
            let fresh19 = sp;
            sp = sp.offset(1);
            let ref mut fresh20 = *(*fresh19 as *mut libc::c_char);
            *fresh20 = a as libc::c_char;
            a = *fresh20 as libc::c_longlong;
        } else if i == PSH as libc::c_int as libc::c_longlong {
            sp = sp.offset(-1);
            *sp = a;
        } else if i == OR as libc::c_int as libc::c_longlong {
            let fresh21 = sp;
            sp = sp.offset(1);
            a = *fresh21 | a;
        } else if i == XOR as libc::c_int as libc::c_longlong {
            let fresh22 = sp;
            sp = sp.offset(1);
            a = *fresh22 ^ a;
        } else if i == AND as libc::c_int as libc::c_longlong {
            let fresh23 = sp;
            sp = sp.offset(1);
            a = *fresh23 & a;
        } else if i == EQ as libc::c_int as libc::c_longlong {
            let fresh24 = sp;
            sp = sp.offset(1);
            a = (*fresh24 == a) as libc::c_int as libc::c_longlong;
        } else if i == NE as libc::c_int as libc::c_longlong {
            let fresh25 = sp;
            sp = sp.offset(1);
            a = (*fresh25 != a) as libc::c_int as libc::c_longlong;
        } else if i == LT as libc::c_int as libc::c_longlong {
            let fresh26 = sp;
            sp = sp.offset(1);
            a = (*fresh26 < a) as libc::c_int as libc::c_longlong;
        } else if i == GT as libc::c_int as libc::c_longlong {
            let fresh27 = sp;
            sp = sp.offset(1);
            a = (*fresh27 > a) as libc::c_int as libc::c_longlong;
        } else if i == LE as libc::c_int as libc::c_longlong {
            let fresh28 = sp;
            sp = sp.offset(1);
            a = (*fresh28 <= a) as libc::c_int as libc::c_longlong;
        } else if i == GE as libc::c_int as libc::c_longlong {
            let fresh29 = sp;
            sp = sp.offset(1);
            a = (*fresh29 >= a) as libc::c_int as libc::c_longlong;
        } else if i == SHL as libc::c_int as libc::c_longlong {
            let fresh30 = sp;
            sp = sp.offset(1);
            a = *fresh30 << a;
        } else if i == SHR as libc::c_int as libc::c_longlong {
            let fresh31 = sp;
            sp = sp.offset(1);
            a = *fresh31 >> a;
        } else if i == ADD as libc::c_int as libc::c_longlong {
            let fresh32 = sp;
            sp = sp.offset(1);
            a = *fresh32 + a;
        } else if i == SUB as libc::c_int as libc::c_longlong {
            let fresh33 = sp;
            sp = sp.offset(1);
            a = *fresh33 - a;
        } else if i == MUL as libc::c_int as libc::c_longlong {
            let fresh34 = sp;
            sp = sp.offset(1);
            a = *fresh34 * a;
        } else if i == DIV as libc::c_int as libc::c_longlong {
            let fresh35 = sp;
            sp = sp.offset(1);
            a = *fresh35 / a;
        } else if i == MOD as libc::c_int as libc::c_longlong {
            let fresh36 = sp;
            sp = sp.offset(1);
            a = *fresh36 % a;
        } else if i == OPEN as libc::c_int as libc::c_longlong {
            a = open(
                *sp.offset(1 as libc::c_int as isize) as *mut libc::c_char,
                *sp as libc::c_int,
            ) as libc::c_longlong;
        } else if i == READ as libc::c_int as libc::c_longlong {
            a = read(
                *sp.offset(2 as libc::c_int as isize) as libc::c_int,
                *sp.offset(1 as libc::c_int as isize) as *mut libc::c_char
                    as *mut libc::c_void,
                *sp as size_t,
            ) as libc::c_longlong;
        } else if i == CLOS as libc::c_int as libc::c_longlong {
            a = close(*sp as libc::c_int) as libc::c_longlong;
        } else if i == PRTF as libc::c_int as libc::c_longlong {
            t = sp.offset(*pc.offset(1 as libc::c_int as isize) as isize);
            a = printf(
                *t.offset(-(1 as libc::c_int) as isize) as *mut libc::c_char,
                *t.offset(-(2 as libc::c_int) as isize),
                *t.offset(-(3 as libc::c_int) as isize),
                *t.offset(-(4 as libc::c_int) as isize),
                *t.offset(-(5 as libc::c_int) as isize),
                *t.offset(-(6 as libc::c_int) as isize),
            ) as libc::c_longlong;
        } else if i == MALC as libc::c_int as libc::c_longlong {
            a = malloc(*sp as libc::c_ulong) as libc::c_longlong;
        } else if i == FREE as libc::c_int as libc::c_longlong {
            free(*sp as *mut libc::c_void);
        } else if i == MSET as libc::c_int as libc::c_longlong {
            a = memset(
                *sp.offset(2 as libc::c_int as isize) as *mut libc::c_char
                    as *mut libc::c_void,
                *sp.offset(1 as libc::c_int as isize) as libc::c_int,
                *sp as libc::c_ulong,
            ) as libc::c_longlong;
        } else if i == MCMP as libc::c_int as libc::c_longlong {
            a = memcmp(
                *sp.offset(2 as libc::c_int as isize) as *mut libc::c_char
                    as *const libc::c_void,
                *sp.offset(1 as libc::c_int as isize) as *mut libc::c_char
                    as *const libc::c_void,
                *sp as libc::c_ulong,
            ) as libc::c_longlong;
        } else if i == EXIT as libc::c_int as libc::c_longlong {
            printf(
                b"exit(%d) cycle = %d\n\0" as *const u8 as *const libc::c_char,
                *sp,
                cycle,
            );
            return *sp;
        } else {
            printf(
                b"unknown instruction = %d! cycle = %d\n\0" as *const u8
                    as *const libc::c_char,
                i,
                cycle,
            );
            return -(1 as libc::c_int) as libc::c_longlong;
        }
    };
}
pub fn main() {
    let mut args: Vec::<*mut libc::c_char> = Vec::new();
    for arg in ::std::env::args() {
        args.push(
            (::std::ffi::CString::new(arg))
                .expect("Failed to convert argument into CString.")
                .into_raw(),
        );
    }
    args.push(::core::ptr::null_mut());
    unsafe {
        ::std::process::exit(
            main_0(
                (args.len() - 1) as libc::c_longlong,
                args.as_mut_ptr() as *mut *mut libc::c_char,
            ) as i32,
        )
    }
}
