importScripts('./worker.js');

const app = Elm.Worker.init();

app.ports.onResult.subscribe((resultGrid) => {
    console.log('onResult', resultGrid);
    self.postMessage({ cmd: 'result', grid: resultGrid });
})

self.addEventListener('message', function(e) {
    var data = e.data;
    switch (data.cmd) {
        case 'run':
            app.ports.run.send({ options: {}, source : data.grid });
        default:
            return;
    }
});
