let numTilesW;
let numTilesH;
let sizeTile = 40;
let tiles = [];
let count;
const stepCount = 2;
let timer = 0;
const timerStep = 1;
const tileVariations = 2;

// change these numbers to change the palette
const colorRange = 120;
const colorStep = 40;

// change these numbers to make animation faster or slower
const stepAmount = 30;
const waitAmount = 20;


async function setup() {
    createCanvas(windowWidth, windowHeight);
    background(0);
    colorMode(HSB, 255);
    noFill();
    strokeWeight(3);
    numTilesW = ceil(width / sizeTile);
    numTilesH = ceil(height / sizeTile);
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

function draw() {
    background(0);
    for (var i = 0; i < numTilesW * numTilesH; i++) {
        tiles[i].display();
    }
    if (timer < stepAmount + waitAmount) {
        timer += timerStep;
    } else {
        timer = 0;
    }
}

function Tile() {
    this.x = 0;
    this.y = 0;
    this.r = sizeTile;
    this.angles = [];
    this.rotation = 0;
    this.values = [];
    this.reverse = false;
    this.needMorph = false;

    this.display = () => {

        push();
            translate(this.x, this.y);
            rotate(radians(this.rotation));
            const currentNoise = this.values[0];
            let t = timer < stepAmount ? timer : stepAmount;
            let incr = this.needMorph ? 1 / stepAmount * t : 0;
            let col = this.values[0] < this.values[1] ? currentNoise + incr : currentNoise - incr;

            stroke(colorStep * col + colorRange, 255, 255);

            if (this.angles[0] === 0 ) {
                arc(-this.r / 2, -this.r / 2, this.r, this.r, 0, PI / 2);
                arc(this.r / 2, this.r / 2, this.r, this.r, -PI, -PI / 2);
            } else {
                arc(-this.r / 2, this.r / 2, this.r, this.r, -PI / 2, 0);
                arc(this.r / 2, -this.r / 2, this.r, this.r, PI / 2, PI);
            }
        pop();

        if (timer < stepAmount && this.needMorph) {
            if (this.reverse) {
                this.rotation -= 90/stepAmount;
            } else {
                this.rotation += 90/stepAmount;
            }
        } else if (timer >= (stepAmount + waitAmount) && this.needMorph) {
            const v1 = this.values[1];
            this.values[1] = this.values[0];
            this.values[0] = v1;
            this.reverse = !this.reverse;
        }
    }
}