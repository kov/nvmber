use std::ffi::{c_char, c_int, CStr};

pub use nvmber::{Error as NvmberError, Nvmber};

#[repr(C)]
#[allow(non_camel_case_types)]
pub enum NvmberResult {
    NVMBER_OK,
    NVMBER_TOO_LARGE,
    NVMBER_MALFORMED,
    NVMBER_INVALID_STR,
    NVMBER_IS_NULL,
}

impl From<NvmberError> for NvmberResult {
    fn from(value: NvmberError) -> Self {
        match value {
            NvmberError::NvmberTooLarge(..) => Self::NVMBER_TOO_LARGE,
            NvmberError::Malformed(..) => Self::NVMBER_MALFORMED,
        }
    }
}

#[no_mangle]
pub extern "C" fn nvmber_new(str: *const c_char, out: *mut *mut Nvmber) -> NvmberResult {
    let Ok(c_str) = unsafe { CStr::from_ptr(str) }.to_str() else {
        return NvmberResult::NVMBER_INVALID_STR;
    };

    match Nvmber::from(c_str) {
        Ok(n) => {
            unsafe {
                *out = Box::into_raw(Box::new(n));
            }
            NvmberResult::NVMBER_OK
        }
        Err(e) => NvmberResult::from(e),
    }
}

#[no_mangle]
pub extern "C" fn nvmber_get_int(nvmber: *mut Nvmber) -> c_int {
    if nvmber.is_null() {
        return -i32::MAX;
    }

    match unsafe { nvmber.as_ref() } {
        Some(n) => n.get_integer() as c_int,
        None => -i32::MAX,
    }
}

#[no_mangle]
pub extern "C" fn nvmber_free(nvmber: *mut Nvmber) {
    if nvmber.is_null() {
        return;
    }

    unsafe {
        drop(Box::from_raw(nvmber));
    }
}

#[no_mangle]
pub extern "C" fn nvmber_sum(a: *mut Nvmber, b: *mut Nvmber) -> *mut Nvmber {
    if a.is_null() || b.is_null() {
        return std::ptr::null_mut();
    }

    Box::into_raw(Box::new(unsafe { &*a + &*b }))
}
