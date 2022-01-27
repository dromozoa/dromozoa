CPPFLAGS =
CFLAGS = -O2 -Wall -W
CXXFLAGS = -O2 -Wall -W -std=c++11
LDFLAGS =

TARGET = mmap code.o

all: $(TARGET)

clean:
	rm -f *.o $(TARGET)

mmap: mmap.o
	$(CXX) $(CPPFLAGS) $(LDFLAGS) $^ -o $@

.c.o:
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $<

.cpp.o:
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $<
