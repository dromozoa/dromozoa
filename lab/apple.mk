CPPFLAGS =
CFLAGS = -O2 -Wall -W -fno-asynchronous-unwind-tables
CXXFLAGS = -O2 -Wall -W -std=c++11
LDFLAGS =

TARGET = mmap code1.s code1.o

all: $(TARGET)

clean:
	rm -f *.o $(TARGET)

mmap: mmap.o
	$(CXX) $(CPPFLAGS) $(LDFLAGS) $^ -o $@

.c.s:
	$(CC) $(CPPFLAGS) $(CFLAGS) -S $<

.c.o:
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $<

.cpp.o:
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $<
