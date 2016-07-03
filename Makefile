compile:
	elm make src/*.elm --output site/game.js

clean:
	rm -f site/game.js

package: compile
	cp -R site snake
	tar -czvf snake.tar.gz snake
	rm -rf snake
	make clean

watch:
	clear
	fswatch -o src/*.elm | xargs -I % bash -c "clear; elm make src/*.elm --output site/game.js"
