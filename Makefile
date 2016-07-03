compile:
	elm make src/*.elm --output site/game.js

clean:
	rm -f site/game.js

watch:
	fswatch -o src/*.elm | xargs -I % elm make src/*.elm --output site/game.js
