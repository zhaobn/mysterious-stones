
/** Settings */
const basePatterns = [ 'plain', 'lt', 'rt'];
const allPatterns = [ 'lt', 'rt', 'ht', 'vt', 'plain' ];

const baseColors = [ 'tomato', 'gold', 'royalblue' ];
const allColors = baseColors.concat(['limegreen']);

/** Rules are hand-crafted in the setRules(agent, target) function
 * If agent has strips: flip recipient strips or add strips
 * Agent colors get passed onto the recipient
 */

const nLearnTasks = 6;
const nGenTasks = 15; // gen := generalization

/** Global variables */
let baseStones = [];
let allStones = [];

baseStones = getAllStones(baseStones, basePatterns, baseColors);
allStones = getAllStones(allStones, allPatterns, allColors);

console.log(baseStones)

const learningTaskConfigs = Array.from(Array(nLearnTasks).keys()).map(k => createConfigs(k+1));
const genTaskConfigs = Array.from(Array(nGenTasks).keys()).map(k => createConfigs(k+1, "generalization"));

let ltData = initDataFile("learn", learningTaskConfigs); // lt := learning tasks
let gtData = initDataFile("gen", genTaskConfigs); // gt := generalization tasks

/** Main page */

for(let i = 0; i < nLearnTasks; i++ ) createLearnTask(learningTaskConfigs[i], (i > 0)? "none": "flex");
// for(let i = 0; i < nGenTasks; i++ ) createGeneralizationTask(genTaskConfigs[i], (i > 0)? "none": "flex");

/** Functions */

function initDataFile (type, configObj) {
  let orig = (type==="learn")? "demo" : "gen";
  let data = {};
  data["task"] = [];
  data["agent"] = [];
  data["recipient"] = [];
  data["result"] = [];

  for (let i = 1; i < 4; i ++) {
    data[`rule_${i}`] = Array.from('-'.repeat(configObj.length));
    data[`confidence_${i}`] = Array.from('-'.repeat(configObj.length));
  }

  configObj.forEach(task => {
    data.task.push(task.index);
    data.agent.push(task[orig].agent);
    data.recipient.push(task[orig].recipient);
    data.result.push(task[orig].result);
  })

  return(data);
}


function createGeneralizationTask (config, display = "flex") {
  let genBox = createCustomElement("div", "box", `genbox-${config.index}`);

  genBox.append(createText('h1', `Task ${config.index}/${nGenTasks}`));
  genBox.append(createTaskBox(config.gen, "flex"));

  genBox.append(createFeedbackText(config.gen, true));
  genBox.append(createFeedbackText(config.gen, false));

  genBox.append(createTextInputPanel(config.gen, "none"));

  document.body.append(genBox);

  const taskNextBtn = document.getElementById(`${config.gen.taskId}-next-btn`);
  const inputForm = document.getElementById(`${config.gen.taskId}-input-form`);
  const inputNextBtn = document.getElementById(`${config.gen.taskId}-input-next-btn`);

  taskNextBtn.onclick = () => showNext(`${config.gen.taskId}-input`);
  inputForm.onchange = () => isFilled(`${config.gen.taskId}-input-form`)? inputNextBtn.disabled = false: null;
  inputNextBtn.onclick= () => {
    gtData = saveFormData(config, gtData);
    (config.index < nGenTasks)? showNext(`genbox-${config.index+1}`) : null;
  }

  genBox.style.display = display;
}


function createLearnTask (config, display = "flex") {
  let learnBox = createCustomElement("div", "box", `box-${config.index}`);

  learnBox.append(createText('h1', `Trial ${config.index}/${nLearnTasks}`))
  learnBox.append(createDemo(config.demo));

  learnBox.append(createTaskBox(config.task, "none"));
  learnBox.append(createFeedbackText(config.task, true));
  learnBox.append(createFeedbackText(config.task, false));

  learnBox.append(createTextInputPanel(config.task, "none"));

  document.body.append(learnBox);

  const playBtn = document.getElementById(`${config.demo.taskId}-play-btn`);
  const demoNextBtn = document.getElementById(`${config.demo.taskId}-next-btn`);
  const taskNextBtn = document.getElementById(`${config.task.taskId}-next-btn`);
  const inputForm = document.getElementById(`${config.task.taskId}-input-form`);
  const inputNextBtn = document.getElementById(`${config.task.taskId}-input-next-btn`);

  playBtn.onclick = () => {
    // (document.getElementById(`${config.demo.taskId}-agent`) === null)? createStones(config.demo): null;
    playEffects(config.demo);
    setTimeout(() => {
      clearElements(config.demo);
      demoNextBtn.disabled = false;
    }, 3000);
  };
  demoNextBtn.onclick = () => showNext(`${config.task.taskId}`);
  taskNextBtn.onclick = () => showNext(`${config.task.taskId}-input`);
  inputForm.onchange = () => isFilled(`${config.task.taskId}-input-form`)? inputNextBtn.disabled = false: null;
  inputNextBtn.onclick= () => {
    ltData = saveFormData(config, ltData);
    (config.index < nLearnTasks)? showNext(`box-${config.index+1}`) : null;
  }
  learnBox.style.display = display;
}

function showNext(nextId) {
  let nextDiv = document.getElementById(nextId);
  nextDiv.style.display = "flex";
  nextDiv.scrollIntoView(true);
}

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

function createConfigs(counter = 1, type = "training") {
  let configs = { "index": counter }

  const agent = sampleStone();
  const recipient = sampleStone();
  const idx = counter.toString().padStart(2, '0');

  if (type === "training") {
    configs["demo"] = {};
    configs["task"] = {};

    configs.demo["taskId"] = "demo" + idx;
    configs.demo["agent"] = agent;
    configs.demo["recipient"] = recipient;
    configs.demo["result"] = setRules(agent, recipient);

    configs.task["taskId"] = "task" + idx;
    configs.task["type"] = "training";
    configs.task["agent"] = agent;
    configs.task["recipient"] = recipient;
    configs.task["result"] = setRules(agent, recipient);

  } else if (type === "generalization") {
    configs["gen"] = {};
    configs.gen["taskId"] = "gen" + idx;
    configs.gen["agent"] = sampleStone(false);
    configs.gen["recipient"] = sampleStone(false);
    configs.gen["result"] = setRules(configs.gen["agent"], configs.gen["recipient"]);
  }

  return(configs);
}

function createDemo (config) {
  const taskBox = createCustomElement("div", "task-box", config.taskId);

  const displayBox = createCustomElement("div", "display-box", `${config.taskId}-display-box`);
  displayBox.append(createDivWithStyle("stone", `${config.taskId}-agent`, config.agent));
  displayBox.append(createDivWithStyle("stone", `${config.taskId}-recipient`, config.recipient));

  const buttonGroup = createCustomElement("div", "button-group", `${config.taskId}-button-group`);
  buttonGroup.append(createBtn(`${config.taskId}-play-btn`, "Play"));
  buttonGroup.append(createBtn(`${config.taskId}-next-btn`, "Next", false));

  taskBox.append(displayBox);
  taskBox.append(buttonGroup);

  return(taskBox);
}

function createTaskBox (config, display = "none") {
  const taskBox = createCustomElement("div", "task-box", `${config.taskId}`);

  const displayBox = createCustomElement("div", "display-box", `${config.taskId}-display-box`);
  displayBox.append(createDivWithStyle("stone", `${config.taskId}-agent`, config.agent));
  displayBox.append(createDivWithStyle("stone", `${config.taskId}-recipient`, config.recipient));

  const recordPan = createCustomElement("div", "record-pan", `${config.taskId}-record-pan`);
  recordPan.append(createPanel(config));

  const buttonGroup = createCustomElement("div", "button-group", `${config.taskId}-button-group`);
  buttonGroup.append(createBtn(`${config.taskId}-check-btn`, "Check", false));
  buttonGroup.append(createBtn(`${config.taskId}-next-btn`, "Next", false));

  taskBox.append(displayBox);
  taskBox.append(recordPan);
  taskBox.append(buttonGroup);

  taskBox.style.display = display;

  return(taskBox);
}

function createTextInputPanel (config, display = "none") {
  const taskBox = createCustomElement("div", "task-box", `${config.taskId}-input`);
  taskBox.setAttribute("style", "height:600px");

  const instructionPan = createCustomElement("div", "instruction", `${config.taskId}-instruction`);
  instructionPan.innerHTML = `
    <h1>Make sure you:</h1>
    <ul>
      <li>Use terms: color, strip, red, orange, blue, left-strip, right-strip.</li>
      <li>Use a functional format, eg. X makes Y (if Z).</li>
      <li>Be clear and descriptive.</li>
    </ul>
    `
  const displayBox = createCustomElement("div", "input-box", `${config.taskId}-input-box`);
  displayBox.append(createInputForm(config));

  const buttonGroup = createCustomElement("div", "button-group", `${config.taskId}-button-group`);
  buttonGroup.append(createBtn(`${config.taskId}-input-next-btn`, "Next", false));

  taskBox.append(instructionPan);
  taskBox.append(displayBox);
  taskBox.append(buttonGroup);

  taskBox.style.display = display;

  return(taskBox);
}

function createFeedbackText (config, pass = false) {
  let text = (pass)? `Well done! Click the "Next" button to proceed.` :
    ((config.taskId.indexOf('gen') > -1)) ? "The correct answer is" :
  `Doesn't look right. Please play the magic effects again, watch carefully, and retry.`;
  let feedbackDiv = createCustomElement("div", `feedback-${pass}`, `${config.taskId}-${pass}-text`);
  feedbackDiv.style.display = "none";
  feedbackDiv.append(document.createTextNode(text))

  if ((config.taskId.indexOf('gen') > -1) && !pass) {
    let correctAnswer = createCustomElement("div", `feedback-stone`, `${config.taskId}-correct-answer`);
    setStyle(correctAnswer, config.result, true);
    feedbackDiv.append(correctAnswer);
  }


  return(feedbackDiv);
}

function createCustomElement (type = 'div', className, id) {
  let element = document.createElement(type);
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

function createInputForm(config) {
  let form = createCustomElement("form", "input-form", `${config.taskId}-input-form`);
  const options = `
    <option value="--" SELECTED>
    <option value="10">10 - Very certain</option>
    <option value="9">9</option>
    <option value="8">8</option>
    <option value="7">7</option>
    <option value="6">6</option>
    <option value="5">5 - Moderately</option>
    <option value="4">4</option>
    <option value="3">3</option>
    <option value="2">2</option>
    <option value="1">1</option>
    <option value="0">0 - Not sure at all</option>
  `
  form.innerHTML = `
    <p>1. What do you think is the most probable magic rule?</p>
    <textarea name="${config.taskId}-input-1" id="${config.taskId}-input-1" placeholder="Please type here"></textarea>
    <p>How certain are you with this rule?
      <select id="${config.taskId}-input-1-certainty" name="${config.taskId}-input-1-certainty" class="input-rule">
        ${options}
      </select>
    </p>
    <p>2. What do you think is the second probable magic rule?</p>
    <textarea name="${config.taskId}-input-2" id="${config.taskId}-input-2" placeholder="Please type here"></textarea>
    <p>How certain are you with this rule?
      <select id="${config.taskId}-input-2-certainty" name="${config.taskId}-input-2-certainty" class="input-rule">
        ${options}
      </select>
    </p>
    <p>3. What do you think is the third probable magic rule?</p>
    <textarea name="${config.taskId}-input-3" id="${config.taskId}-input-3" placeholder="Please type here"></textarea>
    <p>How certain are you with this rule?
      <select id="${config.taskId}-input-3-certainty" name="${config.taskId}-input-3-certainty" class="input-rule">
        ${options}
      </select>
    </p>
    `
  return(form);
}

function playEffects (config) {
  if (!(document.body.contains(document.getElementById(`${config.taskId}-agent`)))) {
    createStones(config)
  }
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
  let btn = createCustomElement("button", className, btnId);
  btn.disabled = !on;
  (text.length > 0) ? btn.append(document.createTextNode(text)): null;
  return(btn)
}

function createStones (config) {
  let el = document.getElementById(`${config.taskId}-display-box`);
  el.append(createDivWithStyle("stone", `${config.taskId}-agent`, config.agent));
  el.append(createDivWithStyle("stone", `${config.taskId}-recipient`, config.recipient));
  return(el)
}

function clearElements (config) {
  let els = [ "agent", "recipient" ].map(s => `${config.taskId}-${s}`);
  els.forEach (el => {
      let clear = document.getElementById(el);
      if(!(clear === null)) clear.parentNode.removeChild(clear);
  })
}

function createPanel(config) {
  const taskId = config.taskId;
  let clicks = [];
  tbs = [];

  stones = (config.type === 'training')? baseStones: allStones;
  nrow = (config.type === 'training')? 3: 5;
  ncol = (config.type === 'training')? 3: 6;

  let tbl = createCustomElement("table", 'selection-panel', `${taskId}-panel`);

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
    clicks.push(clicked);

    let checkBtn = document.getElementById(`${taskId}-check-btn`);
    checkBtn.disabled = false;
    checkBtn.onclick = () => checkSelection(config, clicked.stone);
    // let idx = parseInt(trial.taskId.slice(5,)) - 1;
    // //taskData.trial[idx] = idx + 1;
    // taskData.selection[idx] = readStone(tbId);
    // taskData.ts[idx] = Date.now();
    // taskData.clicks[idx] = clicks;
    // trialData[taskId].selection = clicked;
    // trialData[taskId].clicks.push(clicked);
    // sessionStorage.setItem('taskData', JSON.stringify(taskData))
    styleClicked(tbId);
    //document.getElementById('next-one-btn').disabled = false;
  }

  for(let i = 0; i < nrow; i++){
      let tr = tbl.insertRow();
      for(let j = 0; j < ncol; j++){
        let idx = j + i * ncol;
        let tbId = `tb-${stones[idx]}`;
        tbs.push(tbId)

        let td = tr.insertCell();
        td.setAttribute("id", tbId)

        if(idx < allStones.length) {
          let tc = createCustomElement("div", "panel-stone", tbId.replace(/tb/g, 'ps'));
          setStyle(tc, tc.id.slice(3,), true)

          tc.addEventListener('click', recordClick);
          td.appendChild(tc);
        }
      }
  }
  return tbl;
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

function checkSelection (config, selection) {
  const pass = matchSelections(config.result, selection);
  const passTextDiv = document.getElementById(`${config.taskId}-${pass}-text`);
  passTextDiv.style.display = "block";
  (pass)? document.getElementById(`${config.taskId}-next-btn`).disabled = false :
          document.getElementById(`${config.taskId}-check-btn`).disabled = true;

  setTimeout(() => passTextDiv.style.display = "none",
    (config.taskId.indexOf("gen") < 0 || pass)? 3000: 5000)
}

function matchSelections (stone1, stone2) {
  let s1 = stone1.split("-");
  let s2 = stone2.split("-");

  s1Pattern = s1.shift();
  s2Pattern = s2.shift();

  let patternMatch = (s1Pattern === s2Pattern);
  let colorMatch = true;
  if (s1.length > 1) {
    s1.forEach(s1c => colorMatch && (s2.indexOf(s1c) > -1))
  } else {
    colorMatch = (s1[0] === s2[0])
  }

  return(patternMatch && colorMatch)
}


function isFilled (formID) {
  let notFilled = false;
  const nulls = [ '', '--', '', '--', '', '--' ];
  const form = document.getElementById(formID);
  const inputs = form.elements;
  (Object.keys(inputs)).forEach((input, idx) => {
    let field = inputs[input];
    notFilled = (notFilled || (field.value === nulls[idx]));
    // saveFormData(field, formID);
  });
  return (!notFilled)
}

function saveFormData(config, dataObj) {
  const form = (config.demo === undefined)?
    document.getElementById(`${config.gen.taskId}-input-form`):
    document.getElementById(`${config.task.taskId}-input-form`);

  const inputs = form.elements;
  const idx = config.index - 1;

  (Object.keys(inputs)).forEach((input) => {
    let field = inputs[input];
    let nth = field.name.split("-")[2]
    if (field.name.split("-").length < 4) {
      dataObj[`rule_${nth}`][idx] = field.value;
    } else {
      dataObj[`confidence_${nth}`][idx] = field.value;
    }
  })

  console.log(dataObj);
  return(dataObj);
}
