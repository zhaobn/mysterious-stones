
const mode = "debug"; // dev: show all tasks, debug: skip inputs, prod: final
/** Rules
 * 1. 'rand': random;
 * 2. 'pat': patterned agent makes the recipient the same as the agent.
 * 3. 'flip': patterned agent flips recipient pattern or darkness (if not patterned).
 */
const useRule = "pat";

/** Configurations */
const colorDict = {
  "dark_1": '#EF6C00',
  "dark_2": '#558B2F',
  "light_1": '#FF9800',
  "light_2": '#8BC34A',
}

const basePatterns = [ 'plain', 'lt', 'rt' ];
const allPatterns = [ 'lt', 'rt', 'ht', 'vt', 'plain' ];

const baseColors = [ "dark_1", "light_1", "dark_2", "light_2" ];

const baseStones = getStones(true);
const allStones = getStones(false);

const nLearnTasks = 10;
const learnTaskConfigs = Array.from(Array(nLearnTasks).keys()).map(k => getTaskConfigs(k+1, "learn"));

const nTestTasks = 3;
const testTaskConfigs = Array.from(Array(nTestTasks).keys()).map(k => getTaskConfigs(k+1, "test"));

const nGenTasks = 10; // gen := generalization
const genTaskConfigs = Array.from(Array(nGenTasks).keys()).map(k => getTaskConfigs(k+1, "gen"));

let ltData = initDataFile(learnTaskConfigs); // lt := learning tasks
let gtData = initDataFile(genTaskConfigs); // gt := generalization tasks

let textSelection = '';

/** Main body */
if (mode !== "dev") {
  document.body.append(createCustomElement("div", "section-page", "show-learning-phase"));
  document.getElementById("show-learning-phase").append(createText("h1", "Experiment starts"));

  document.body.append(createCustomElement("div", "section-page", "show-test-phase"));
  document.getElementById("show-test-phase").append(createText("h1", "Tests"));
  document.getElementById("show-test-phase").style.display = "none";

  document.body.append(createCustomElement("div", "section-page", "show-gen-phase"));
  document.getElementById("show-gen-phase").append(createText("h1", "Generalization tasks"));
  document.getElementById("show-gen-phase").style.display = "none";
}

// createTaskBox(genTaskConfigs[0]);
for(let i = 0; i < nLearnTasks; i++ ) {
  createTaskBox(learnTaskConfigs[i], (mode === "dev")? "flex" : "none");
}
for(let i = 0; i < nTestTasks; i++ ) {
  createTaskBox(testTaskConfigs[i], (mode === "dev")? "flex" : "none");
}
for(let i = 0; i < nGenTasks; i++ ) {
  createTaskBox(genTaskConfigs[i], (mode === "dev")? "flex" : "none");
}

if (mode !== "dev") {
  setTimeout(() => {
    document.getElementById("show-learning-phase").style.display = "none";
    document.getElementById("box-learn-01").style.display = "flex";
  }, 2000);
}

/** functions */
function createInitStones(config, parentDiv) {
  parentDiv.append(createDivWithStyle("stone", `${config.taskId}-agent`, config.agent));
  parentDiv.append(createDivWithStyle("stone", `${config.taskId}-recipient`, config.recipient));
  return(parentDiv);
}

function createSummaryStones(config, parentDiv) {
  let stoneSets = createCustomElement("div", "stone-sets", `${config.taskId}-stone-sets`);

  stoneSets.append(createDivWithStyle("stone-before", `${config.taskId}-summary-recipient`, config.recipient));
  stoneSets.append(createDivWithStyle("stone", `${config.taskId}-summary-result`, config.result))

  parentDiv.append(createDivWithStyle("stone", `${config.taskId}-summary-agent`, config.agent));
  parentDiv.append(stoneSets);
  return(parentDiv);
}


function createTaskBox (config, display = "none") {
  const taskType = config.type;
  const taskId = config.taskId;

  let index = config.index;
  let total = 0;
  // const total = nLearnTasks + nTestTasks + nGenTasks;

  switch (taskType) {
    case 'learn':
      total = nLearnTasks;
      break;
    case 'test':
      total = nTestTasks;
      break;
    case 'gen':
      total = nGenTasks;
      break;
  }

  let box = createCustomElement("div", "box", `box-${taskId}`);
  box.append(createText('h1', `
    ${mode === 'dev'? "["+ taskType + "]": ''}
    ${index}/${total}`));

  let taskBox = createCustomElement("div", "task-box", `taskbox-${taskId}`);
  let displayBox = createCustomElement("div", "display-box", `${taskId}-display-box`);
  displayBox = createInitStones(config, displayBox);

  const buttonGroup = createCustomElement("div", "button-group", `${taskId}-button-group`);
  if (taskType !== "learn") {
    buttonGroup.append(createBtn(`${taskId}-check-btn`, "Check", false))
  } else {
    buttonGroup.append(createBtn(`${taskId}-play-btn`, "Play", true))
  }

  if (taskType !== "learn") {
    const recordPan = createCustomElement("div", "record-pan", `${taskId}-record-pan`);
    recordPan.append(createPanel(config));

    taskBox.append(displayBox);
    taskBox.append(recordPan);
    taskBox.append(buttonGroup);

  } else {
    taskBox.append(displayBox);
    taskBox.append(buttonGroup);
  }

  box.append(taskBox);

  if (taskType !== "learn") {
    const feedbackPass = createCustomElement("div", "feedback-true", `${taskId}-true-text`);
    feedbackPass.append(document.createTextNode("Correct! See above for the effects summary."))
    feedbackPass.style.display = "none";

    const feedbackFail = createCustomElement("div", "feedback-false", `${taskId}-false-text`);
    feedbackFail.append(document.createTextNode("Wrong! See above for the real effects summary."));
    feedbackFail.style.display = "none";

    box.append(feedbackPass);
    box.append(feedbackFail);
  }

  box.append(createTextInputPanel(config, (mode === "dev" || mode === "debug")? "flex": "none"));

  document.body.append(box);
  box.style.display = display;

  /** Button functionalities */
  const playBtn = document.getElementById(`${taskId}-play-btn`) || null;
  const inputForm = document.getElementById(`${taskId}-input-form`);
  const copyBtn = document.getElementById(`${taskId}-copy-btn`);
  const pasteBtn = document.getElementById(`${taskId}-paste-btn`);
  const inputNextBtn = document.getElementById(`${taskId}-input-next-btn`);

  copyBtn.onclick = () => copyText(`${taskId}-input-1`);
  pasteBtn.onclick = () => pasteText(`${taskId}-input-1`);

  if (taskType === "learn") {
    playBtn.onclick = () => {
      playBtn.disabled = true;
      playEffects(config);
      setTimeout(() => {
        clearElements(config);
        setTimeout(() => {
          displayBox = createSummaryStones(config, displayBox);
          showNext(`${taskId}-input`);
        }, 1000);
      }, 5000);
    }
  }

  inputForm.onchange = () => isFilled(`${taskId}-input-form`)? inputNextBtn.disabled = false: null;

  inputNextBtn.onclick= () => {
    inputNextBtn.disabled = true;
    (taskType === "gen")? gtData = saveFormData(config, gtData) : ltData = saveFormData(config, ltData);
    disableFormInputs(`${taskId}-input-form`);
    copyBtn.disabled = false;
    pasteBtn.disabled = true;

    const taskCount = parseInt(taskId.split("-")[1]);
    if (taskType === "learn") {
      if(taskCount < nLearnTasks) {
        showNext(`box-learn-${fmtTaskIdx(taskCount+1)}`)
      } else {
        for(let i = 0; i < nLearnTasks; i ++) document.getElementById(`box-learn-${fmtTaskIdx(i+1)}`).style.display = "none";
        document.getElementById("show-test-phase").style.display = "block";
        setTimeout(() => {
          document.getElementById("show-test-phase").style.display = "none";
          document.getElementById("box-test-01").style.display = "flex";
        }, 2000);
      }
    } else if (taskType === "test") {
      if(taskCount < nTestTasks) {
        showNext(`box-test-${fmtTaskIdx(taskCount+1)}`)
      } else {
        for(let i = 0; i < nTestTasks; i ++) document.getElementById(`box-test-${fmtTaskIdx(i+1)}`).style.display = "none";
        document.getElementById("show-gen-phase").style.display = "block";
        setTimeout(() => {
          document.getElementById("show-gen-phase").style.display = "none";
          document.getElementById("box-gen-01").style.display = "flex";
        }, 2000);
      }
    } else {
      if(taskCount < nGenTasks) {
        showNext(`box-gen-${fmtTaskIdx(taskCount+1)}`)
      } else {
        alert("This is the last task.")
      }
    }
  }
}

function fmtTaskIdx (counter) {
  return(counter.toString().padStart(2, '0'))
}
function readTaskIdx (taskId) {
  return(parseInt(taskId.split('-')[1]))
}

function getTaskConfigs(counter = 1, type = "learn") {
  let configs = {};

  configs["taskId"] = type + "-" + fmtTaskIdx(counter);
  configs["type"] = type;
  configs["index"] = counter;
  configs["agent"] = sampleStone();
  configs["recipient"] = sampleStone();
  configs["result"] = setRules(configs["agent"], configs["recipient"], useRule);

  return(configs);
}

function getStones (isBase) {
  let stones = []
  const colors = baseColors;
  const patterns = isBase? basePatterns: allPatterns;
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

function copyText (id) {
  textSelection = document.getElementById(id).value;
}

function pasteText (id) {
  const textArea = document.getElementById(id);
  let text = textArea.value;
  text = text + " " + textSelection;
  textArea.value = text;
}

function sampleStone (isBase = true) {
  let stoneStyle = '';

  patterns = isBase? basePatterns : allPatterns;
  colors = baseColors;
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

/** Rules
 * 1. 'rand': random;
 * 2. 'pat': patterned agent makes the recipient the same as the agent.
 * 3. 'flip': patterned agent flips recipient pattern or darkness (if not patterned).
 */
function setRules (agent, recipient, ruleNumber = "rand") {
  let resultStone = '';

  const agts = agent.split("-");
  const rcps = recipient.split("-");

  const agentPattern = agts.shift();
  const recipientPattern = rcps.shift();

  if (ruleNumber === 'rand') {
    resultStone = sampleStone();

  } else if (ruleNumber === 'pat') {
    resultStone = (agentPattern !== 'plain')? agent : recipient;

  } else if (ruleNumber === 'flip') {
    let result = [];
    if (agentPattern !== 'plain') {
      if (recipientPattern !== 'plain') {
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
        result.push(rcps[0]);
        result.push(rcps[1]);
      } else {
        result.push("plain");
        result.push(setDarkness(rcps[0]))
      }
    } else {
      result = agent.split("-");
      // result.push(recipientPattern);
      // const agentDarkness = agts[0].split('_')[0];
      // result.push(setDarkness(rcps[0],agentDarkness ));
      // (recipientPattern !== "plain")? result.push(setDarkness(rcps[1], agentDarkness)) : null;
    }
    resultStone = result.join("-");
  }
  return(resultStone);
}

function setDarkness (str, opt = "flip") {
  const darkness = str.split("_")[0];
  const color = str.split("_")[1];
  let resultDarkness = darkness;
  if (opt === 'flip') {
    resultDarkness = (darkness === "dark")? "light": "dark";
  } else {
    resultDarkness = opt;
  }
  return(`${resultDarkness}_${color}`)
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

function createTextInputPanel (config, display = "none") {
  const taskBox = createCustomElement("div", "task-box", `${config.taskId}-input`);
  // taskBox.setAttribute("style", "height:600px");

  const instructionPan = createCustomElement("div", "instruction", `${config.taskId}-instruction`);
  instructionPan.innerHTML = `
    <h1>Make sure you:</h1>
    <ul>
      <li>Refer to objects as <b>Agent</b>, <b>Recipient</b>, and <b>Result</b>.</li>
      <li>Refer to object properties using <b>dark/pale</b>, <b>red</b>, <b>blue</b>, <b>plain</b>, <b>left stripes</b>, <b>right stripes</b>.</li>
    </ul>
    `
  const editBtns = createCustomElement("div", "edit-buttons", `${config.taskId}-edit-btns`);
  editBtns.append(createBtn(`${config.taskId}-copy-btn`, "Copy", false));
  editBtns.append(createBtn(`${config.taskId}-paste-btn`, "Paste",
    (config.type === "learn" && config.index === 1)? false : true));

  const displayBox = createCustomElement("div", "input-box", `${config.taskId}-input-box`);
  displayBox.append(createInputForm(config));
  displayBox.append(editBtns);

  const buttonGroup = createCustomElement("div", "button-group", `${config.taskId}-button-group`);
  buttonGroup.append(createBtn(`${config.taskId}-input-next-btn`, "Next",
    (mode === "dev" || mode === "debug")? true: false));
  // buttonGroup.append(createBtn(`${config.taskId}-copy-btn`, "Copy", false));
  // buttonGroup.append(createBtn(`${config.taskId}-paste-btn`, "Paste", true));

  // taskBox.append(instructionPan);
  taskBox.append(displayBox);
  taskBox.append(buttonGroup);

  taskBox.style.display = display;

  return(taskBox);
}

function setStyle (el, styleStr, isSmall = false) {
  const fill = styleStr.split('-')[0];
  const color1 = colorDict[styleStr.split('-')[1]];
  const color2 = colorDict[styleStr.split('-').length > 2 ? styleStr.split('-')[2] : ''];

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
    <p>What do you think is the cause of this observation?</p>
    <textarea name="${config.taskId}-input-1" id="${config.taskId}-input-1" placeholder="Please type here"></textarea>
    <p>How certain are you with this?
      <select id="${config.taskId}-input-1-certainty" name="${config.taskId}-input-1-certainty" class="input-rule">
        ${options}
      </select>
    </p>
    `
  return(form);
}


function createPanel(config) {
  const taskId = config.taskId;
  let clicks = [];
  let tbs = [];

  stones = baseStones;
  nrow = 4;
  ncol = 4;

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
    if ((document.getElementById(`${config.taskId}-input`).style.display === 'none') || mode === 'dev') {
      checkBtn.disabled = false;
    }
    checkBtn.onclick = () => checkSelection(config, clicked.stone);
    styleClicked(tbId);
  }

  for(let i = 0; i < nrow; i++){
    let tr = tbl.insertRow();
    for(let j = 0; j < ncol; j++){
      let idx = j + i * ncol;
      let tbId = (idx < stones.length)? `${taskId}-tb-${stones[idx]}` : `${taskId}-tb-blank-${idx - stones.length}`;
      tbs.push(tbId)
      let td = tr.insertCell();
      td.setAttribute("id", tbId)

      if(idx < stones.length) {
        let tc = createCustomElement("div", "panel-stone", tbId.replace(/tb/g, 'ps'));
        const tcStyle = tc.id.split('-').slice(3,).join('-')
        setStyle(tc, tcStyle, true)

        tc.addEventListener('click', recordClick);
        td.appendChild(tc);
      } else {
        let tc = createCustomElement("div", "blank", tbId.replace(/tb/g, 'ps'));
        td.appendChild(tc);
      }
    }
  }
  return tbl;
}

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
  const selected = selection.split("-").slice(3,).join("-");
  const pass = matchSelections(config.result, selected);

  const passTextDiv = document.getElementById(`${config.taskId}-${pass}-text`);
  passTextDiv.style.display = "block";

  document.getElementById(`${config.taskId}-check-btn`).disabled = true;
  clearElements(config);
  setTimeout(() => {
    let displayBox = document.getElementById(`${config.taskId}-display-box`);
    displayBox = createSummaryStones(config, displayBox);
    showNext(`${config.taskId}-input`);
  }, 1000)
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
  });
  return (!notFilled)
}

function showNext(nextId) {
  let nextDiv = document.getElementById(nextId);
  nextDiv.style.display = "flex";
  nextDiv.scrollIntoView(mode === 'dev'? false: true);
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

function clearElements (config) {
  let els = [ "agent", "recipient" ].map(s => `${config.taskId}-${s}`);
  els.forEach (el => {
      let clear = document.getElementById(el);
      if(!(clear === null)) clear.parentNode.removeChild(clear);
  })
}

function initDataFile (configs) {
  let data = {};
  data["type"] = [];
  data["task"] = [];
  data["agent"] = [];
  data["recipient"] = [];
  data["result"] = [];
  data["report"] = [];
  data["confidence"] = [];

  // for (let i = 1; i < 4; i ++) {
  //   data[`rule_${i}`] = Array.from('-'.repeat(configObj.length));
  //   data[`confidence_${i}`] = Array.from('-'.repeat(configObj.length));
  // }

  configs.forEach(config => {
    data.type.push(config.taskId.split('-')[0]);
    data.task.push(readTaskIdx(config.taskId).toString());
    data.agent.push(config.agent);
    data.recipient.push(config.recipient);
    data.result.push(config.result);
  })

  return(data);
}

function saveFormData(config, dataObj) {
  const form = document.getElementById(`${config.taskId}-input-form`);
  const inputs = form.elements;
  (Object.keys(inputs)).forEach((input) => {
    let field = inputs[input];
    if (field.name.indexOf("certainty") < 0) {
      dataObj["report"].push(field.value);
    } else {
      dataObj["confidence"].push(field.value);
    }
  })
  return(dataObj);
}

function disableFormInputs (formId) {
  const form = document.getElementById(formId);
  const inputs = form.elements;
  (Object.keys(inputs)).forEach((input) => inputs[input].disabled = true);
}
