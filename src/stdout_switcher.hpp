#include "stdout_switcher.h"

stdout_switcher::stdout_switcher() {
    fd = 0;
}
void stdout_switcher::switch_stdout(const char *newStream)
{
    if(newStream==NULL)
        newStream = tmpnam(NULL);
    fflush(stdout);
    fgetpos(stdout, &pos);
    fd = dup(fileno(stdout));
    freopen(newStream, "w", stdout);
}
const char* stdout_switcher::revert_stdout()
{
    fflush(stdout);
    long len = ftell(stdout);
    buf.resize(len);
    fseek(stdout, 0, SEEK_SET);
    size_t rdlen = fread((void*)buf.data(), 1, len, stdout);
    //result[rdlen] = 0;
    dup2(fd, fileno(stdout));
    close(fd);
    //clearerr(fileno(stdout));
    fsetpos(stdout, &pos);
    return buf.c_str();
}
