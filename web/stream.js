var source = new EventSource('/stream');

// Connection opened
source.addEventListener('open', function (event) {
    console.log('Connected to server');
}, false);

// Listen for messages
source.addEventListener('message', function (event) {
    let json = JSON.parse(event.data);

    console.log('Message from server: ', json);
    
   // pass json to function that will update the DOM
    inputEvent(json);
}, false);

// Connection error
source.addEventListener('error', function (event) {
    if (event.target.readyState == EventSource.CLOSED) {
        console.log('Disconnected from server');
    } else if (event.target.readyState == EventSource.CONNECTING) {
        console.log('Reconnecting to server...');
    } else {
        console.error('Error: ', event);
    }
}, false);

var devices = {};

function inputEvent(json) {
    let id = json.deviceId;
    let name = json.deviceName;
    let state = json.state;
    let continuations = json.continuations;
    
    // Get or create device
    if (!devices[id]) {
        devices[id] = {
            id: id,
            name: name,
            states: [],
            log: [],
            inputs: {},
            outputs: {},
            timers: []
        };
    }

    let device = devices[id];

    // Update device state
    device.state = state;

    device.log = [];

    // set all inputs to inactive
    Object.values(device.inputs).forEach(function (input) {
        input.active = false;
        input.update = false;
    });

    // set all outputs to inactive
    Object.values(device.outputs).forEach(function (output) {
        output.active = false;
        output.update = false;
    });

    // delete all timers
    device.timers = [];

    continuations.forEach(function (continuation) {
        let type = continuation.type;

        // GroupValue (ga), GroupRead (ga), Scheduled (timerId, time (ISO 8601))

        if (type == 'GroupValue') {
            let key = continuation.ga;

            // check if input exists, else create it
            if (!device.inputs[key]) {
                device.inputs[key] = {
                    value: null,
                    active: false
                }
            } else 
                device.inputs[key].active = true;

            device.inputs[key].update = true;
        } else if (type == 'GroupRead') {
            let key = continuation.ga;

            // check if output exists, else create it
            if (!device.outputs[key]) {
                device.outputs[key] = {
                    value: null,
                    active: true
                }
            } else
                device.outputs[key].active = true;

            device.outputs[key].update = true;
        } else if (type == 'Scheduled') {
            let timerId = continuation.timerId;
            let time = new Date(continuation.time);

            // add timer
            device.timers.push({
                timerId: timerId,
                time: time
            });
        }
    });


    json.log.forEach(function (log) {
        let type = log.type;

        if (type == 'Log') {
            device.log.push(log.message);
        } else if (type == 'KNXIn') {
            let key = log.ga;
            let value = showDpt(log.dpt);

            device.inputs[key] = {
                value: value,
                active: true
            }
        } else if (type == 'KNXOut') {
            let key = log.ga;
            let value = showDpt(log.dpt);

            if (!device.outputs[key]) {
                device.outputs[key] = {
                    value: value
                }
            } else
                device.outputs[key].value = value;

            device.outputs[key].update = true;
        }
    });

    updateDevices(devices);
}

function showDpt(dpt) {
    if (dpt.type == 'DPT9' || dpt.type == 'DPT5_1') {
        // round value to 2 decimals
        let value = Math.round(dpt.value * 100) / 100;
        return value; // + ' (' + dpt.type + ')';
    }

    return dpt.value; // + ' (' + dpt.type + ')';
}

var interval = null;

function updateDevices(devicesDict) {
    let devices = Object.values(devicesDict);
    const deviceContainer = d3.select('#devices');
    const sections = deviceContainer.selectAll('section').data(devices, d => d.id);

    const newSections = sections.enter().append('section');
    newSections.append('h2').attr('class', 'device-title');
    const divIO = newSections.append('div').attr('class', 'io-container');
    divIO.append('div').attr('class', 'inputs').append('table');
    divIO.append('div').attr('class', 'outputs').append('table');
    newSections.append('div').attr('class', 'timers');
    newSections.append('div').attr('class', 'state');
    newSections.append('ul').attr('class', 'logs');

    const allSections = newSections.merge(sections);

    allSections.select('.device-title').text(device => device.name);
    allSections.select('.inputs table').each((device, i, nodes) => updateIO(d3.select(nodes[i]), device.inputs, true));
    allSections.select('.outputs table').each((device, i, nodes) => updateIO(d3.select(nodes[i]), device.outputs, false));
    allSections.select('.state').text(device => JSON.stringify(device.state, null, 1));
    allSections.select('.logs').each((device, i, nodes) => updateLogs(d3.select(nodes[i]), device.log));

    const timers = allSections.select('.timers').selectAll('div').data(device => device.timers, d => d.timerId);
    const newTimers = timers.enter().append('div');
    newTimers.append('span').attr('class', 'timer-time');
    const allTimers = newTimers.merge(timers);

    const removeTimers = timers.exit();
    removeTimers.remove();

    allTimers.select('.timer-time').text(timer => timerString(timer.time));

    if (interval)
        clearInterval(interval);

    interval = setInterval(function () {
        allTimers.select('.timer-time').text(timer => timerString(timer.time));
    }, 1000);

    function timerString (time) {
        let now = new Date();
        let diff = time - now;
        let seconds = Math.round(diff / 1000);
        let minutes = Math.floor(seconds / 60);
        let hours = Math.floor(minutes / 60);
        let days = Math.floor(hours / 24);

        seconds = seconds % 60;
        minutes = minutes % 60;
        hours = hours % 24;

        // remove left most zeros
        let timeString = '';
        if (days > 0)
            timeString += days + 'd ' + hours + 'h ' + minutes + 'm ' + seconds + 's';
        else if (hours > 0)
            timeString += hours + 'h ' + minutes + 'm ' + seconds + 's';
        else if (minutes > 0)
            timeString += minutes + 'm ' + seconds + 's';
        else
            timeString += seconds + 's';

        return timeString;
    }

    sections.exit().remove();

    function flashAnimation(selection) {
        selection.classed('flash', true);
        selection.transition().duration(3000).on('end', () => selection.classed('flash', false));
    }

    function updateIO(table, ioData, isInput) {
        const ioKeys = Object.keys(ioData).sort();
        const ioArray = ioKeys.map(key => ({key: key, value: ioData[key]}));
        const rows = table.selectAll('tr').data(ioArray, d => d.key);
    
        // Enter selection
        const newRows = rows.enter().append('tr');
        newRows.append('td');
        newRows.append('td');
    
        // Update selection
        const allRows = newRows.merge(rows);
        allRows.select('td:nth-child(1)').text(d => isInput ? d.value.value : d.key);
        allRows.select('td:nth-child(2)').text(d => isInput ? d.key : d.value.value);
    
        // Handle active class
        allRows.classed('active', d => d.value.active);
        
        // If update, trigger animation
        flashAnimation(allRows.filter(d => d.value.update));
    
        // Exit selection
        rows.exit().remove();
    }

    function updateLogs(ul, logsData) {
        const logs = logsData.map(log => ({message: log}));
        const lis = ul.selectAll('li').data(logs, d => d.message);

        // Enter selection
        const newLis = lis.enter().append('li');
        
        // Update selection
        const allLis = newLis.merge(lis);
        allLis.text(d => d.message);

        // Exit selection
        lis.exit().remove();
    }
}
