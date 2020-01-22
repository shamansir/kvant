

const express = require('express')
const cors = require('cors')
const app = express()
const port = 3000

app.use(cors())
app.use(express.static('./assets'))

app.listen(port, () => console.log(`Serving static files at port ${port}!`))
