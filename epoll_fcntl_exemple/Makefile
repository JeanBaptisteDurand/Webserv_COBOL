COB = cobc
FLAGS = -x -free
SRCDIR = src
TARGET = webserver

SRC = $(wildcard $(SRCDIR)/*.cob)
 
all: $(TARGET)

$(TARGET): $(SRC)
	$(COB) $(FLAGS) -o $(TARGET) $(SRC)

clean:
	rm -f $(TARGET) *.o
