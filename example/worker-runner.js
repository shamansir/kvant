importScripts('./worker.js');

self.addEventListener('message', function(e) {
    var data = e.data;
    switch (data.cmd) {
        case 'start':
            var app = Elm.Worker.init();
            console.log(app.ports);
        default:
            return;
    }
});
