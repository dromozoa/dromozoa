CPPFLAGS =
CFLAGS = -O2 -Wall -W -fno-asynchronous-unwind-tables
CXXFLAGS = -O2 -Wall -W -std=c++11
LDFLAGS =

TARGET = mmap code1.s code1.o code2a.s code2b.s code2c.s code2

all: $(TARGET)

clean:
	rm -f *.o $(TARGET)

mmap: mmap.o
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

code2: code2a.o code2b.o code2c.o
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $^ -o $@

.c.s:
	$(CC) $(CPPFLAGS) $(CFLAGS) -S $<

.c.o:
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $<

.cpp.o:
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $<
