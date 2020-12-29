let numTilesW;
let numTilesH;
let sizeTile = 40;
let tiles = [];
let n = 0;
const stepCount = 2;
const stepAmount = 60;
let timer = 0;
const timerStep = 1;
const waitAmount = 60;

function setup() {
    createCanvas(windowWidth, windowHeight);
    background(0);
    colorMode(HSB, 255);
    noFill();
    strokeWeight(3);
    numTilesW = ceil(width / sizeTile);
    numTilesH = ceil(height / sizeTile);
    const count = numTilesW * numTilesH;

    for (var a = 0; a < stepCount; a++) {
        app.ports.triggerCalculation.send({
            count,
            rules: [],
            size: [numTilesW, numTilesH],
        });
    };

     app.ports.sendOutput.subscribe(function(message) {
        for (var i = 0; i < numTilesW; i++) {
            for (var j = 0; j < numTilesH; j++) {

                if (tiles.length < count) {
                    tiles.push(new Tile);
                    tiles[j + i * numTilesH].x = i * sizeTile + sizeTile / 2;
                    tiles[j + i * numTilesH].y = j * sizeTile + sizeTile / 2;
                    tiles[j + i * numTilesH].col = [0.04 * j, 0.04 * i];
                    tiles[j + i * numTilesH].values.push(message[j][i]/1000);
                    tiles[j + i * numTilesH].orientation.push(message[j][i]/1000 > 0.5 ? 90 : 0);
                } else {
                    tiles[j + i * numTilesH].values.push(message[j][i]/1000);
                    tiles[j + i * numTilesH].orientation.push(message[j][i]/1000 > 0.5 ? 90 : 0);
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
    this.x;
    this.y;
    this.r = sizeTile;
    this.orientation = [];
    this.rotation = 0;
    this.rotating = false;
    this.values = [];
    this.reverse = false;

    this.display = () => {

        if (this.orientation[0] !== this.orientation[1]) {
            this.rotating = true;
        }

        push();
            translate(this.x, this.y);
            rotate(radians(this.rotation));
            const currentNoise = this.values[0];
            let t = timer < stepAmount ? timer : stepAmount;
            let incr = abs(this.values[0] - this.values[1]) / stepAmount * t;
            let col = this.values[0] < this.values[1] ? currentNoise + incr : currentNoise - incr;

            stroke(100 * col + 80, 255, 255);

            if (this.orientation[0] === 0 ) {
                arc(-this.r / 2, -this.r / 2, this.r, this.r, 0, PI / 2);
                arc(this.r / 2, this.r / 2, this.r, this.r, -PI, -PI / 2);
            } else {
                arc(-this.r / 2, this.r / 2, this.r, this.r, -PI / 2, 0);
                arc(this.r / 2, -this.r / 2, this.r, this.r, PI / 2, PI);
            }
        pop();


        if (timer < stepAmount) {
            if (this.rotating) {
                if (this.reverse) {
                    this.rotation -= 90/stepAmount;
                } else {
                    this.rotation += 90/stepAmount;
                }
            }
        } else if (timer >= (stepAmount + waitAmount)) {
            const v1 = this.values[1];
            this.values[1] = this.values[0];
            this.values[0] = v1;
            this.reverse = !this.reverse;
        }
    }
}