
/** Configurations */
const borderWidth = "4px";
const mar = 5;
const len = 40;

const colorDict = {
  "very_dark": "#052e54",
  "dark": "#1155cc",
  "medium": "#6d9eeb",
  "light": "#c9daf8",
}

// let exp_configs = []

// const fixed_obj = 42
// let varied_obj = [ 52, 53, 51, 31, 41, 32, 33, 43, 63, 62 ].sort()
// varied_obj = shuffle(varied_obj)


// /** Generate experiment configs */
// varied_obj.forEach((r, idx) => {
//   exp_configs.push(formatConfig(idx, 'A1', 'learn', fixed_obj, r, 'AA'))
// })

// getGenObjs('A1').forEach((pair, idx) => {
//   exp_configs.push(formatConfig(idx, 'A1', 'gen', pair[0], pair[1]))
// })

// varied_obj.forEach((r, idx) => {
//   exp_configs.push(formatConfig(idx, 'A2', 'learn', r, fixed_obj, 'AA'))
// })

// getGenObjs('A2').forEach((pair, idx) => {
//   exp_configs.push(formatConfig(idx, 'A2', 'gen', pair[0], pair[1]))
// })

// varied_obj.forEach((r, idx) => {
//   exp_configs.push(formatConfig(idx, 'A3', 'learn', fixed_obj, r, 'AB'))
// })

// getGenObjs('A3').forEach((pair, idx) => {
//   exp_configs.push(formatConfig(idx, 'A3', 'gen', pair[0], pair[1]))
// })

// varied_obj.forEach((r, idx) => {
//   exp_configs.push(formatConfig(idx, 'A4', 'learn', r, fixed_obj, 'AB'))
// })

// getGenObjs('A4').forEach((pair, idx) => {
//   exp_configs.push(formatConfig(idx, 'A4', 'gen', pair[0], pair[1]))
// })


/** Debugging */
// console.log(JSON.stringify(exp_configs))
showSetup('A1', 'edges(A)+1, shades(R)+1; fixed A')
showLearnTasks('A1', 'learn')
showLearnTasks('A1', 'gen')

showSetup('A2', 'edges(A)+1, shades(R)+1; fixed R')
showLearnTasks('A2', 'learn')
showLearnTasks('A2', 'gen')

showSetup('A3', 'edges(R)+1, shades(A)+1; fixed A')
showLearnTasks('A3', 'learn')
showLearnTasks('A3', 'gen')

showSetup('A4', 'edges(R)+1, shades(A)+1; fixed R')
showLearnTasks('A4', 'learn')
showLearnTasks('A4', 'gen')



/** Helper function */
function showLearnTasks (group, phase) {
  const learnTasks = exp_configs.filter(o => o.group==group & o.phase==phase)

  let tableDiv = document.getElementById(`${phase}-table-div-${group}`)
  let table = createCustomElement('table', 'setup', `${phase}-table-${group}`)

  let header = table.insertRow();
  [ 'Trial', 'Agent', 'Recipient', 'Result' ].forEach(el => {
    header.insertCell().appendChild(createText('p', el))
  })

  learnTasks.forEach(task => {
    let taskRow = table.insertRow();

    taskRow.insertCell().appendChild(createText('p', task.trial));
    taskRow.insertCell().appendChild(createStone(task.agent, task.id, 1));
    taskRow.insertCell().appendChild(createStone(task.recipient, task.id, 0));

    (phase=='learn') ?
      taskRow.insertCell().appendChild(createStone(task.result, task.id, 0)) :
      taskRow.insertCell().appendChild(createText('p', '?'));
  })

  tableDiv.append(table)
}

function createStone (config, id, isAgent) {
  // translate config to polygon options
  const orderedColors = ['light', 'medium', 'dark', 'very_dark']
  const shade = orderedColors[config % 10 - 1]
  const edges = Math.floor(config/10)

  let div = createCustomElement("div", "", `${id}-div`);
  let svg = createCustomElement("svg", "setup-stone", `${id}-svg`);
  let polygon = createCustomElement("polygon", "shape", `${id}-stone`);
  setAttributes(polygon, {
    "fill": colorDict[shade],
    "points": calcPolygon(edges),
    "stroke-width": isAgent? borderWidth : "0px",
  });

  svg.append(polygon)
  div.append(svg)
  return(div);
}


function showSetup (group, text) {
  const header = `${group}: ${text}`
  let box = createCustomElement('div', 'setup' ,`box-${group}`)
  box.innerHTML = `
  <h1>${header}</h1>
  <div class="divider">
    <div id="learn-${group}">
      <h2>Learning trials</h2>
      <div class='table-div' id='learn-table-div-${group}'></div>
    </div>
    <div id="gen-${group}">
      <h2>Generalization trials</h2>
      <div class='table-div' id='gen-table-div-${group}'></div>
    </div>
  </div>
  `
  document.body.append(box)
}


function getGenObjs (group) {
  let pairs = [];
  let agents = [];
  let recipients = [];

  let fixed_sameShades = [ 32, 52 ]
  let fixed_sameEdges = [ 41, 43 ]
  let fixed_totalDiff = [ 33, 61 ]
  let fixed_notOccured = fixed_sameShades.concat(fixed_sameEdges).concat(fixed_totalDiff)
  let fixed_tests = shuffle(fixed_notOccured.concat(fixed_obj))

  let varied_notOccured = [ 42, 61 ]

  switch (group) {
    case 'A1':
      agents = fixed_tests;
      recipients = shuffle(varied_notOccured.concat(72));
      break;
    case 'A2':
      agents = shuffle(varied_notOccured.concat(34));
      recipients = fixed_tests;
      break;
    case 'A3':
      agents = fixed_tests;
      recipients = shuffle(varied_notOccured.concat(34));
      break;
    case 'A4':
      agents = shuffle(varied_notOccured.concat(72));
      recipients = fixed_tests;
      break;
  }

  agents.forEach(a => {
    recipients.forEach(r => pairs.push([a, r]))
  })

  return shuffle(pairs)
}

function formatConfig (idx, group, phase, agent, recipient, rule) {
  let config = {};
  config['id'] = `${group}-${phase}-${padNum(idx+1)}`
  config['group'] = group;
  config['phase'] = phase;
  config['trial'] = idx+1,
  config['agent'] = agent;
  config['recipient'] = recipient;
  config['result'] = fetchResult(agent, recipient, rule);
  return config
}

function fetchResult (agent, recipient, rule) {
  const readShades = (obj) => obj % 10
  const readEdges = (obj) => Math.floor(obj / 10)
  switch (rule) {
    case "AA":
      return (readEdges(agent)+1) * 10 + (readShades(recipient)+1);
    case "AB":
      return (readEdges(recipient)+1) * 10 + (readShades(agent)+1);
    default:
      return 0
  }
}

function shuffle (array) {
  for (let i = array.length - 1; i > 0; i--) {
    let j = Math.floor(Math.random() * (i + 1));
    [array[i], array[j]] = [array[j], array[i]];
  }
  return array
}

function padNum (num) {
  return num.toString().padStart(2, '0')
}

function createCustomElement (type = 'div', className, id) {
  const svgElements = [ "svg", "polygon", "circle", "rect", "path" ];
  let element = (svgElements.indexOf(type) < 0)?
    document.createElement(type):
    document.createElementNS("http://www.w3.org/2000/svg", type);
  if (className.length > 0) element.setAttribute("class", className);
  element.setAttribute("id", id);
  return element;
}
function createDivWithStyle (className = "div", id = "", style = "") {
  let element = createCustomElement('div', className, id);
  setStyle(element, style);
  return element;
}
function createText(h = "h1", text = 'hello') {
  let element = document.createElement(h);
  let tx = document.createTextNode(text);
  element.append(tx);
  return(element)
}
function setAttributes(el, attrs) {
  for(var key in attrs) {
    el.setAttribute(key, attrs[key]);
  }
}

function calcPolygon(n) {
  let output = [];
  let adjust = (n===5)? 55 : 0;

  if (n === 3) {
    output.push(`${len/2},${mar}`);
    output.push(`${len-mar},${len-mar}`);
    output.push(`${mar},${len-mar}`);
  } else if (n === 4) {
    output.push(`${mar},${mar}`);
    output.push(`${len-mar},${mar}`);
    output.push(`${len-mar},${len-mar}`);
    output.push(`${mar},${len-mar}`);
  } else {
    // Adapted from https://gist.github.com/jonthesquirrel/e2807811d58a6627ded4
    for (let i = 1; i <= n; i++) {
      output.push(
        ((len/2 * Math.cos(adjust + 2 * i * Math.PI / n)) + len/2).toFixed(0).toString() + "," +
        ((len/2 * Math.sin(adjust + 2 * i * Math.PI / n)) + len/2).toFixed(0).toString()
      )
    }
  }
  return output.join(" ")
}
