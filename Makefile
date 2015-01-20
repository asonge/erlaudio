CC = cc

CFLAGS = -g -O3 -pedantic -Wall -Wextra -Wno-unused-parameter -Wno-missing-field-initializers -fPIC
LDFLAGS = -bundle -flat_namespace -undefined suppress

ERLANG_PATH = $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)
ERLANG_LIBPATH = $(shell erl -eval 'io:format("~s", [[code:lib_dir(erl_interface),"/lib"]])' -s init stop -noshell)

CFLAGS += -I$(ERLANG_PATH)
LDFLAGS += -L$(ERLANG_LIBPATH) -lerl_interface -lei

UNAME = $(shell uname)
ifeq ($(UNAME), Darwin)
	CFLAGS += -I/opt/local/include
	LDFLAGS += -L/opt/local/lib
endif

LDFLAGS += -lportaudio

all: priv/erlaudio_drv.so

c_src/erlaudio_drv.o: c_src/erlaudio_drv.h c_src/erlaudio_drv.c
	$(CC) -c $(CFLAGS) -o c_src/erlaudio_drv.o c_src/erlaudio_drv.c

priv/erlaudio_drv.so: c_src/erlaudio_drv.o
	$(CC) c_src/erlaudio_drv.o $(LDFLAGS) -o $@

clean:
	rm -f priv/erlaudio_drv.so
	rm -f c_src/erlaudio_drv.o
