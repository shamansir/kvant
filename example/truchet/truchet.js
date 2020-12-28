let numTilesW;
let numTilesH;
let sizeTile = 40;
let tiles = [];
let n = 0;
let kvantData = [];

async function setup() {
    createCanvas(windowWidth, windowHeight);
    background(0);
    colorMode(HSB, 255);
    noFill();
    strokeWeight(3);
    numTilesW = ceil(width / sizeTile);
    numTilesH = ceil(height / sizeTile);
    const count = numTilesW * numTilesH;

    await app.ports.triggerCalculation.send({
         count,
         rules: [],
         size: [numTilesW, numTilesH],
    });

    await app.ports.sendOutput.subscribe(function(message) {
        kvantData = message;

        for (var i = 0; i < numTilesW; i++) {
            for (var j = 0; j < numTilesH; j++) {
                tiles.push(new Tile);

                tiles[j + i * numTilesH].x = i * sizeTile + sizeTile / 2;
                tiles[j + i * numTilesH].y = j * sizeTile + sizeTile / 2;
                tiles[j + i * numTilesH].col = [0.04 * j, 0.04 * i];
                tiles[j + i * numTilesH].data = kvantData[j][i]/1000;
            }
        }
    });
}

function draw() {
    background(0);
    for (var i = 0; i < numTilesW * numTilesH; i++) {
        tiles[i].display();
    }
}

function Tile() {
    this.x;
    this.y;
    this.r = sizeTile;
    this.orientation = random();
    this.rotation = 0;
    this.rotating = false;
    this.col;


    this.display = () => {
        push();
        translate(this.x, this.y);
        rotate(radians(this.rotation));
        const currentNoise = this.data;
        stroke(100 * currentNoise + 80, 255, 255);
        if (this.orientation > 0.5) {
            arc(-this.r / 2, -this.r / 2, this.r, this.r, 0, PI / 2);
            arc(this.r / 2, this.r / 2, this.r, this.r, -PI, -PI / 2);
        } else {
            arc(-this.r / 2, this.r / 2, this.r, this.r, -PI / 2, 0);
            arc(this.r / 2, -this.r / 2, this.r, this.r, PI / 2, PI);
        }
        pop();

        noLoop();

        if (currentNoise.toFixed(2) == 0.25
            || currentNoise.toFixed(2) == 0.45
            || currentNoise.toFixed(2) == 0.65
        ) {
            this.rotating = true;
        }

        this.data += 0.001;
        if (this.rotating) {
            this.rotation += 3;
            if (this.rotation % 90 === 0) {
                this.rotating = false;
            }
        }
    }
}