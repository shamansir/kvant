let numTilesW;
let numTilesH;
let sizeTile = 40;
let tiles = [];
let count;
const stepCount = 2;
let timer = 0;
const timerStep = 1;
const tileVariations = 3;

// change these numbers to change the palette
const colorRange = 120;
const colorStep = 40;

// change these numbers to make animation faster or slower
const stepAmount = 30;
const waitAmount = 20;

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

async function setup() {
    createCanvas(windowWidth, windowHeight);
    background(0);
    colorMode(HSB, 255);
    noFill();
    strokeWeight(3);
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