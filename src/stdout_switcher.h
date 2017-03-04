#pragma once

#include <string>
#include <stdio.h>
#include <unistd.h>

class stdout_switcher {
    int fd;
    fpos_t pos;
    std::string buf;
public:
    stdout_switcher();
    void switch_stdout(const char *newStream = NULL);
    const char* revert_stdout();
};
