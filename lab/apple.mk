CPPFLAGS =
CXXFLAGS = -g -O2 -Wall -W -std=c++11
LDFLAGS =

TARGET = mmap

all: $(TARGET)

clean:
	rm -f *.o $(TARGET)

mmap: mmap.o
	$(CXX) $(CPPFLAGS) $(LDFLAGS) $^ -o $@

.cpp.o:
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $<
