var source = new EventSource('/stream');
var devices = {};

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

function inputEvent(json) {
    let {deviceId: id, deviceName: name, state, continuations, log} = json;

    // Reset all device states
    for (let device of Object.values(devices)) {
        resetDeviceUpdate(device);
    }

    // Get or create device
    let device = devices[id] = devices[id] || {
        id,
        name,
        states: [],
        log: [],
        inputs: {},
        outputs: {},
        timers: []
    };

    // Update device state
    device.state = state;

    // Reset device state
    resetDeviceState(device);

    // Process continuations
    processContinuations(device, continuations);

    // Process logs
    processLogs(device, log);

    updateDevices(devices);
}

function resetDeviceUpdate(device) {
    for (let input of Object.values(device.inputs)) {
        input.update = false;
    }

    for (let output of Object.values(device.outputs)) {
        output.update = false;
    }
}

function resetDeviceState(device) {
    for (let input of Object.values(device.inputs)) {
        input.active = false;
    }

    for (let output of Object.values(device.outputs)) {
        output.active = false;
    }

    // Clear logs and timers
    device.log = [];
    device.timers = [];
}

function processContinuations(device, continuations) {
    for (let continuation of continuations) {
        let {type, ga, timerId, time} = continuation;

        if (type === 'GroupValue' || type === 'GroupRead') {
            let io = type === 'GroupValue' ? device.inputs : device.outputs;

            io[ga] = io[ga] || { value: null };
            io[ga].active = true;
        } else if (type === 'Scheduled') {
            device.timers.push({ timerId, time: new Date(time) });
        }
    }
}

function processLogs(device, logs) {
    logs.forEach(log => {
        let {type, ga, dpt} = log;

        if (type === 'Log') {
            device.log.push(log.message);
        } else if (type === 'KNXIn' || type === 'KNXOut') {
            let io = type === 'KNXIn' ? device.inputs : device.outputs;

            io[ga] = io[ga] || { value: null };
            io[ga].value = showDpt(dpt);
            io[ga].update = true;
        }
    });
}

function showDpt(dpt) {
    if (dpt.type == 'DPT9' || dpt.type == 'DPT5_1') {
        // round value to 2 decimals
        let value = Math.round(dpt.value * 100) / 100;
        return value; // + ' (' + dpt.type + ')';
    }

    if (dpt.type == 'DPT10') {
        // TimeOfDay
        let {knxWeekDay, knxTimeOfDay} = dpt.value;

        // split at . and take first part
        let time = knxTimeOfDay.split('.')[0];

        return time + " (" + knxWeekDay + ")";
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

    // update state, but only if not empty array
    allSections.select('.state').text(device => Object.keys(device.state).length > 0 ? JSON.stringify(device.state, null, 2) : '');

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
