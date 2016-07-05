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
	find src | grep elm$  | xargs fswatch -o | xargs -I % bash -c "clear; elm make src/Game.elm --output site/game.js"
