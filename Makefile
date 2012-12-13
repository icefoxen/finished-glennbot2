CHICKEN=csc
FLAGS=

SOURCE=glennbot.scm channel.scm datastruct.scm util.scm
OBJECTS=glennbot.o channel.o datastruct.o util.o
OUTPUT=glennbot

all: clean $(OUTPUT) 

clean:
	rm -f $(OBJECTS)
	rm -f *~

$(OUTPUT): $(OBJECTS)
	$(CHICKEN) -o $(OUTPUT) $(OBJECTS) 

$(OBJECTS): $(SOURCE)
	$(CHICKEN) $(FLAGS) -c $(SOURCE)
