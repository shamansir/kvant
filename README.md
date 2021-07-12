# wfc-elm

## Run example locally

```bash
npm install -g elm elm-live
cd ./example
npm install
npm run serve & # in background, or in a separate terminal
npm run build
npm start
```

Alternative, in different terminals:

```bash
cd ./example
npm install
npm run start:worker # in the first terminal
npm run start:mehanik # in the second terminal
npm run serve # in the third terminal
```

Navigate http://localhost:8080

## Run example in Docker

```bash
docker build . -t kvant-example
docker run -p 1235:8080 kvant-example
```

Navigate http://localhost:1235
