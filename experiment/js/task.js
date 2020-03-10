
/** Settings */
const basePatterns = [ 'plain', 'lt', 'rt'];
const allPatterns = basePatterns.concat(['ht', 'vt']);

const baseColors = [ 'tomato', 'darkorange', 'royalblue' ];
const allColors = baseColors.concat(['green']);

// Rules:
// 1. Colors: to same color
// 2. Strips: add same strips or flip stripps
// Hard-coded in the setRules(agent, target) functions

let baseStones = []
let allStones = []

baseStones = getAllStones(baseStones, basePatterns, baseColors);

let demo = { "taskId": "demo" };
demo = createDemoConfig(demo);
let tasks = [];
tasks = createTaskConfig(tasks, demo);
const task01 = tasks[0];

/** Main page */
console.log(demo.agent);
console.log(demo.recipient);
console.log(setRules(demo.agent, demo.recipient));
document.body.append(createDemo(demo));
document.getElementById(`${demo.taskId}-play-btn`).onclick = () => {
  (document.getElementById(`${demo.taskId}-agent`) === null)? createStones(demo): null;
  playEffects(demo);
  setTimeout(() => {
    clearElements(demo);
    document.getElementById(`${demo.taskId}-next-btn`).disabled = false;
  }, 3000);
};
document.getElementById(`${demo.taskId}-next-btn`).onclick = () => {
  (document.getElementById(`${task01.taskId}-display-box`) === null)?
    document.body.append(createTaskBox(task01)) : null;
}

/** Functions */
function sampleStone (isBase = true) {
  let stoneStyle = '';

  patterns = isBase? basePatterns : allPatterns;
  colors = isBase? baseColors : allColors;
  const pt = patterns[Math.floor(Math.random() * patterns.length)];
  const color1 = colors[Math.floor(Math.random() * colors.length)];

  if (pt === 'plain') {
    stoneStyle = pt + "-" + color1
  } else {
    const colorsLeft = colors.filter(c => c != color1);
    const color2 = colorsLeft[Math.floor(Math.random() * colorsLeft.length)];
    stoneStyle = pt + "-" + color1 + "-" + color2;
  }
  return(stoneStyle);
}

function getAllStones (stones, patterns, colors) {
  patterns.forEach(p => {
    if (p === 'plain') {
      colors.forEach(c => stones.push([p, c].join('-')))
    } else {
      let colorsToUse = colors;
      colors.forEach(c => {
        colorsToUse = colorsToUse.filter(cu => cu != c);
        colorsToUse.forEach(ct => stones.push([p, c, ct].join("-")))
      })
    }
  })
  return(stones);
}

function getDiffColors (agentColors, recipientColors) {
  let diffs = recipientColors.filter(rc => rc != agentColors);
  let diff = (diffs.length > 1)? diffs[Math.floor(Math.random() * diffs.length)]: diffs[0];
  return(diff);
}
/** Rules
 * If agent has strips: flip recipient strips or add strips
 * Agent colors get passed onto the recipient
 */
function setRules (agent, recipient) {
  let result = []
  const agts = agent.split("-");
  const rcps = recipient.split("-");

  let agentPattern = agts.shift();
  let recipientPattern = rcps.shift();

  if (agentPattern !== "plain") {
    if (recipientPattern === 'plain') {
      result.push(agentPattern)
      result.push(rcps[0])
      result.push(getDiffColors(rcps[0], agts))
    } else {
      switch(recipientPattern) {
        case 'lt':
          result.push('rt');
          break;
        case 'rt':
          result.push('lt');
          break;
        case 'vt':
          result.push('ht');
          break;
        case 'ht':
          result.push('vt');
          break;
      }
      result.push(rcps[0])
      result.push(rcps[1])
    }
  } else {
    result.push(recipientPattern);
    result.push(agts[0]);
    if (recipientPattern !== 'plain') {
      let c = getDiffColors(agts[0], rcps);
      result.push(c)
    }
  }
  return result.join("-");
}

function createDemoConfig (demoObj = demo) {
  const agent = sampleStone();
  const recipient = sampleStone();
  const result = sampleStone(); // TODO: proper function
  demoObj["taskId"] = "demo";
  demoObj["agent"] = agent;
  demoObj["recipient"] = recipient;
  demoObj["result"] = setRules(agent, recipient);
  return(demoObj);
}

function createTaskConfig(taskArr, demoObj, type = 'training') {
  const counter = taskArr.length + 1;
  let task = {};
  task["taskId"] = "task" + counter.toString().padStart(2, '0');
  task["type"] = type;
  task["agent"] = demoObj.agent;
  task["recipient"] = demoObj.recipient;
  task["result"] = setRules(demoObj.agent, demoObj.recipient);
  taskArr.push(task);
  return(taskArr);
}

function createDemo (config) {
  const taskBox = createDivWithClassId("task-box", config.taskId);

  const displayBox = createDivWithClassId("display-box", `${config.taskId}-display-box`);
  displayBox.append(createDivWithStyle("stone", `${config.taskId}-agent`, config.agent));
  displayBox.append(createDivWithStyle("stone", `${config.taskId}-recipient`, config.recipient));

  const buttonGroup = createDivWithClassId("button-group", `${config.taskId}-button-group`);
  buttonGroup.append(createBtn(`${config.taskId}-play-btn`, "Play"));
  buttonGroup.append(createBtn(`${config.taskId}-next-btn`, "Next", false));

  taskBox.append(displayBox);
  taskBox.append(buttonGroup);

  return(taskBox);
}

function createTaskBox (config) {
  const taskBox = createDivWithClassId("task-box", `${config.taskId}`);
  taskBox.append(document.createTextNode(`${config.taskId.slice(4,)}.`));

  const displayBox = createDivWithClassId("display-box", `${config.taskId}-display-box`);
  displayBox.append(createDivWithStyle("stone", `${config.taskId}-agent`, config.agent));
  displayBox.append(createDivWithStyle("stone", `${config.taskId}-recipient`, config.recipient));

  const recordPan = createDivWithClassId("record-pan", `${config.taskId}-record-pan`);
  recordPan.append(createPanel(config));

  const buttonGroup = createDivWithClassId("button-group", `${config.taskId}-button-group`);
  buttonGroup.append(createBtn(`${config.taskId}-check-btn`, "Check"));
  buttonGroup.append(createBtn(`${config.taskId}-next-btn`, "Next"));

  taskBox.append(displayBox);
  taskBox.append(recordPan);
  taskBox.append(buttonGroup);

  return(taskBox);
}

function createDivWithClassId (className = "div", id = "") {
  let element = document.createElement('div');
  element.setAttribute("class", className);
  element.setAttribute("id", id);
  return element;
}

function createDivWithStyle (className = "div", id = "", style = "") {
  let element = document.createElement('div');
  element.setAttribute("class", className);
  element.setAttribute("id", id);
  setStyle(element, style);
  return element;
}

function addText (id, text) {
  let el = document.getElementById(id);
  const t = document.createTextNode(text);
  el.appendChild(t);
}

function playEffects (config, isInit = true) {
  !isInit? createStones(config): null;
  moveStone(config);
  changeStone(config);
}

function moveStone (config) {
  const agent = `${config.taskId}-agent`;
  const recipient = `${config.taskId}-recipient`;

  const agentStone = document.getElementById(agent);
  const startPos = getCurrentLocation(agent).right;
  const endPos = getCurrentLocation(recipient).left;

  const delta = Math.round(endPos - startPos);
  (delta > 0) && (agentStone.style.left = `${delta}px`);
}

function changeStone (config) {
  const recipientStone = document.getElementById(`${config.taskId}-recipient`);
  setTimeout(() => {
    setStyle(recipientStone, config.result);
  }, 1500);
}

function getCurrentLocation(id) {
  let rect = {top: 0, bottom: 0, left: 0, right: 0};
  const pos = document.getElementById(id).getBoundingClientRect();
  rect.top = pos.top;
  rect.bottom = pos.bottom;
  rect.left = pos.left;
  rect.right = pos.right;
  return rect;
}

function setStyle (el, styleStr, isSmall = false) {
  const fill = styleStr.split('-')[0];
  const color1 = styleStr.split('-')[1];
  const color2 = styleStr.split('-').length > 2 ? styleStr.split('-')[2] : '';

  const len = isSmall? 5: 15;

  switch (fill) {
    case "plain":
      el.style.background = color1;
      break;
    case "lt":
      el.style.background = `repeating-linear-gradient(
        -45deg, ${color1}, ${color1} ${len}px, ${color2} ${len}px, ${color2} ${2 * len}px
      )`;
      break;
    case "rt":
      el.style.background = `repeating-linear-gradient(
        45deg, ${color1}, ${color1} ${len}px, ${color2} ${len}px, ${color2} ${2 * len}px
      )`;
      break;
    case "ht":
      el.style.background = `repeating-linear-gradient(
        0deg, ${color1}, ${color1} ${len}px, ${color2} ${len}px, ${color2} ${2 * len}px
      )`;
      break;
    case "vt":
      el.style.background = `repeating-linear-gradient(
        90deg, ${color1}, ${color1} ${len}px, ${color2} ${len}px, ${color2} ${2 * len}px
      )`;
      break;
  }
}

function createBtn (btnId, text = "Button", on = true, className = "task-button") {
  let btn = document.createElement("button");
  btn.setAttribute("class", className);
  btn.setAttribute("id", btnId);
  btn.disabled = !on;
  (text.length > 0) ? btn.append(document.createTextNode(text)): null;
  return(btn)
}

function createStones (config, box = ".display-box") {
  let el = document.querySelector(box);
  el.append(createDivWithStyle("stone", `${config.taskId}-agent`, config.agent));
  el.append(createDivWithStyle("stone", `${config.taskId}-recipient`, config.recipient));
  return(el)
}

function clearElements (config) {
  let els = [ "agent", "recipient" ].map(s => `${config.taskId}-${s}`);
  els.forEach (el => {
      let clear = document.getElementById(el);
      clear.parentNode.removeChild(clear);
  })
}

function createPanel(config) {
  const taskId = config.taskId;
  let clicks = [];
  tbs = [];

  stones = (config.type === 'training')? baseStones: allStones;
  ncol = (config.type === 'training')? 3: 5;
  nrow = (config.type === 'training')? 3: 4;

  let tbl = document.createElement('table');
  setAttributes(tbl, { 'class': 'selection-panel', 'id': `${taskId}-panel` })

  const styleClicked = (id) => {
    const selectedTb = id.replace(/ps/g, 'tb');
    tbs.forEach(tbid => {
      hover (tbid, selectedTb);
      const tb = document.getElementById(tbid);
      tb.style.border = (tbid === selectedTb)? '4px solid #e0e0e0' : '0px';
    })
  }

  const recordClick = (e) => {
    const tbId = e.target.id;
    let clicked = {};
    clicked.stone = tbId.slice(3,);
    clicked.timestamp = Date.now();
    clicks.push(clicked)
    // let idx = parseInt(trial.taskId.slice(5,)) - 1;
    // //taskData.trial[idx] = idx + 1;
    // taskData.selection[idx] = readStone(tbId);
    // taskData.ts[idx] = Date.now();
    // taskData.clicks[idx] = clicks;
    console.log(clicks)
    // trialData[taskId].selection = clicked;
    // trialData[taskId].clicks.push(clicked);
    // sessionStorage.setItem('taskData', JSON.stringify(taskData))
    styleClicked(tbId);
    //document.getElementById('next-one-btn').disabled = false;
  }

  for(let i = 0; i < nrow; i++){
      let tr = tbl.insertRow();
      for(let j = 0; j < ncol; j++){
        let tbId = `tb-${stones[j + i * nrow]}`;
        tbs.push(tbId)

        let td = tr.insertCell();
        setAttributes(td, {'id': tbId})

        let tc = document.createElement('div');
        setAttributes(tc, {
            'class': "panel-stone",
            'id': tbId.replace(/tb/g, 'ps'),
          })
        setStyle(tc, tc.id.slice(3,), true)

        tc.addEventListener('click', recordClick);
        td.appendChild(tc);
      }
  }
  return tbl;
}

function setAttributes(el, attrs) {
  for(var key in attrs) {
    el.setAttribute(key, attrs[key]);
  }
}

/** Fake hover effects for selection panel */
function hover (tbid, selected) {
  const tb = document.getElementById(tbid);
  tb.onmouseover = function() {
      (tbid !== selected)? this.style.border = '3px solid #e0e0e0' : null;
  };
  tb.onmouseleave = function() {
      (tbid !== selected)? this.style.border = '0px' : null;
  };
}
