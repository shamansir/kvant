importScripts('./worker.js');

const app = Elm.Worker.init();

app.ports.onResult.subscribe((resultWave) => {
    console.log('onResult', resultWave);
    self.postMessage({ cmd: 'result', wave: resultWave });
});

app.ports.onStep.subscribe((stepWave) => {
    console.log('onStep', stepWave);
    self.postMessage({ cmd: 'step', wave: stepWave });
});

app.ports.onPatterns.subscribe((patterns) => {
    console.log('onPatterns', patterns);
    self.postMessage({ cmd: 'patterns', patterns: patterns });
});

self.addEventListener('message', function(e) {
    var data = e.data;
    switch (data.cmd) {
        case 'run':
            app.ports.run.send({ options: data.options, source : data.source });
            break;
        case 'trace':
            app.ports.trace.send({ options: data.options, source : data.source });
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
        case 'preprocess':
            app.ports.preprocess.send({ options: data.options, source : data.source });
            break;
        case 'matches':
            app.ports.matchesAt.send(data.position);
            break;
        default:
            return;
    }
});
