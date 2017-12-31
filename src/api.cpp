#include <vector> 

#include <R.h>

#define R_INTERFACE_PTRS
#include <Rinterface.h>

#include "api.hh"

SEXP C_try_readline ()
{
    std::vector<char> buf(1024), tmp(1024);

    int ret = ptr_R_ReadConsole(">> ", reinterpret_cast<unsigned char*>(buf.data()), buf.size(), 0);

    int len = snprintf(tmp.data(), tmp.size(), "ret = %d\n", ret);
//    ptr_R_WriteConsole(tmp.data(), len);
    printf("%s\n", tmp.data());

    len = snprintf(tmp.data(), tmp.size(), "buf = [%s]\n", buf.data());
    printf("%s\n", tmp.data());
//    ptr_R_WriteConsole(tmp.data(), len);

    return R_NilValue;
}
