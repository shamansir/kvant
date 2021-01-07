importScripts('./worker.js');

const app = Elm.Worker.init();

app.ports.onResult.subscribe((resultGrid) => {
    console.log('onResult', resultGrid);
    self.postMessage({ cmd: 'result', grid: resultGrid });
});

app.ports.onStep.subscribe((stepGrid) => {
    console.log('onStep', stepGrid);
    self.postMessage({ cmd: 'step', grid: stepGrid });
});

self.addEventListener('message', function(e) {
    var data = e.data;
    switch (data.cmd) {
        case 'run':
            app.ports.run.send({ options: {}, source : data.grid });
            break;
        case 'trace':
            app.ports.trace.send({ options: {}, source : data.grid });
            break;
        case 'step':
            app.ports.step.send(null);
            break;
        case 'back':
            app.ports.back.send(null);
            break;
        case 'stop':
            app.ports.stop.send(null);
            break;
        default:
            return;
    }
});
