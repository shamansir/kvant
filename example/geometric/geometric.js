async function preload() {
    numW = ceil(width / sizeTile);
    numH = ceil(height / sizeTile);

    actualSize = width / numW;
    numTilesH = width / numH;

    count = numTilesW * numTilesH;

    for (let a = 0; a < stepCount; a++) {
        await app.ports.triggerCalculation.send({
            count: tileVariations,
            rules: [],
            size: [numTilesW, numTilesH],
        });
    };

    await app.ports.sendOutput.subscribe(function(message) {
        for (let i = 0; i < numTilesW; i++) {
            for (let j = 0; j < numTilesH; j++) {

                if (tiles.length < count) {
                    tiles.push(new Tile);
                    const tile = tiles[j + i * numTilesH];

                    tile.x = i * sizeTile + sizeTile / 2;
                    tile.y = j * sizeTile + sizeTile / 2;
                    tile.values.push(message[j][i]);
                    tile.angles.push(message[j][i] * 90);
                } else {
                    const tile = tiles[j + i * numTilesH];

                    tile.values.push(message[j][i]);
                    tile.angles.push(message[j][i] * 90);
                    tile.needMorph = tile.values[0] !== tile.values[1];
                }
            }
        }
    });
}



const palettes = {
    JetBrains: ["#ed3d7d", "#7c59a4", "#fcee39"],
    Space: ["#003CB7", "#5FCCF5", "#ADF03E"],
    IntelliJ: ["#007efc", "#fe315d", "#f97a12"],
    PhpStorm: ["#b345f1", "#765af8", "#ff318c"],
    PyCharm: ["#21d789", "#fcf84a", "#07c3f2"],
    RubyMine: ["#fe2857", "#fc801d", "#9039d0"],
    WebStorm: ["#07c3f2", "#087cfa", "#fcf84a"],
    CLion: ["#21d789", "#009ae5", "#ed358c"],
    DataGrip: ["#22d88f", "#9775f8", "#ff318c"],
    AppCode: ["#087cfa", "#07c3f2", "#21d789"],
    GoLand: ["#0d7bf7", "#b74af7", "#3bea62"],
    ReSharper: ["#c21456", "#e14ce3", "#fdbc2c"],
    ReSharperCpp: ["#fdbc2c", "#e14ce3", "#c21456"],
    DotCover: ["#ff7500", "#7866ff", "#e343e6"],
    DotMemory: ["#ffbd00", "#7866ff", "#e343e6"],
    DotPeek: ["#00caff", "#7866ff", "#e343e6"],
    DotTrace : ["#fc1681", "#786bfb", "#e14ce3"],
    Rider: ["#c90f5e", "#077cfb", "#fdb60d"],
    TeamCity: ["#0cb0f2", "#905cfb", "#3bea62"],
    YouTrack: ["#0cb0f2", "#905cfb", "#ff318c"],
    Upsource: ["#22b1ef", "#9062f7", "#fd8224"],
    Hub: ["#00b8f1", "#9758fb", "#ffee45"],
    Kotlin: ["#22b1ef", "#9062f7", "#fd8224"],
    MPS: ["#0b8fff", "#21d789", "#ffdc52"],
    IntelliJEdu: ["#0d7bf7", "#fe315d", "#f97a12"],
    PyCharmEdu: ["#21d789", "#fcf84a", "#07c3f2"],
    Datalore: ["#3bea62", "#6b57ff", "#07c3f2"],
}


let width, height, box;
let tilesize = 40, amount;
let amountX, amountY;
let exported;


// helpers
const randRGB = () => (Math.floor(Math.random()*255));
const randInArr = array => (array[Math.floor(Math.random() * array.length)])

const tiles = [
    '<polygon points="100 100 0 0 100 0 100 100"/>',
    '<path d="M100,100H0V0A100,100,0,0,1,100,100Z"/>',
    '<rect width="40" height="40"/>',
    '<path d="M50,20A30,30,0,1,0,80,50,30,30,0,0,0,50,20Zm0,40A10,10,0,1,1,60,50,10,10,0,0,1,50,60Z"/>',
    `<circle cx="50" cy="20" r="10"/>
    <circle cx="50" cy="80" r="10"/>
    <circle cx="80" cy="50" r="10"/>
    <circle cx="20" cy="50" r="10"/>`,
    '<path d="M50,0a50,50,0,1,0,50,50V0Zm0,80A30,30,0,1,1,80,50,30,30,0,0,1,50,80Z"/>',
    '<rect x="40" y="40" width="60" height="60"/>',
    `<rect y="20" width="100" height="20"/>
    <rect y="60" width="100" height="20"/>,`
]

function setup() {
    width = window.innerWidth;
    height = window.innerHeight;
    amountX = Math.ceil(width / tilesize);
    amountY = Math.ceil(height / tilesize);

    box = SVG().addTo('body').size(width, height);
}

function draw() {
    makeGrid();
    exported = box.svg();
}

function makeGrid() {
    for (let x = 0; x < width; x += tilesize) {
        for (let y = 0; y < height; y += tilesize) {
            const group = box.group();
            // const col = new SVG.Color({ r: randRGB(), g: randRGB(), b: randRGB() });
            const hex = randInArr(palettes['DataGrip']);
            group.svg(randInArr(tiles));
            group.move(x, y).fill(hex).size(tilesize, tilesize);
        }
    }
}

function downloadSVG(text, fileType, fileName) {
    var blob = new Blob([text], { type: fileType });

    var a = document.createElement('a');
    a.download = fileName;
    a.href = URL.createObjectURL(blob);
    a.dataset.downloadurl = [fileType, a.download, a.href].join(':');
    a.style.display = "none";
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    setTimeout(function() { URL.revokeObjectURL(a.href); }, 1500);
}


document.addEventListener('keypress', e => {
    if (e.code === 'Space') downloadSVG(exported, 'text/xml', 'geometric.svg');
});

setup();
draw();