

const express = require('express')
const app = express()
const port = 3000

app.use(express.static('assets'))

app.listen(port, () => console.log(`Serving static files at port ${port}!`))
