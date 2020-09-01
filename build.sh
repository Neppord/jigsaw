rm -rf docs/*
elm make --optimize src/Main.elm --output=docs/index.html
cp -r resources/ docs/resources