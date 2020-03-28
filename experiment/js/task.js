
const mode = "dev" // set to '' for prod

/** Configurations */
const colorDict = {
  "dark_1": 'darkred',
  "dark_2": 'navy',
  "dark_3": 'darkolivegreen',
  "light_1": 'plum',
  "light_2": 'cornflowerblue',
  "light_3": 'limegreen',
}

const basePatterns = [ 'plain', 'lt', 'rt' ];
const allPatterns = [ 'lt', 'rt', 'ht', 'vt', 'plain' ];

const baseColors = [ "dark_1", "light_1", "dark_2", "light_2" ];

const baseStones = getStones(true);
const allStones = getStones(false);

const nLearnTasks = 1;
const learnTaskConfigs = Array.from(Array(nLearnTasks).keys()).map(k => getTaskConfigs(k+1), "learn");

const nGenTasks = 20; // gen := generalization
const genTaskConfigs = Array.from(Array(nGenTasks).keys()).map(k => getTaskConfigs(k+1, "gen"));


/** Main body */
// createTaskBox(genTaskConfigs[0]);
for(let i = 0; i < nLearnTasks; i++ ) createTaskBox(learnTaskConfigs[i], (i === 0)? "flex" : "none");
for(let i = 0; i < nGenTasks; i++ ) createTaskBox(genTaskConfigs[i], "none");

/** functions */

function createTaskBox (config, display = "none") {
  console.log(config);

  const isGenTask = config.taskId.split('-')[0] === "gen";
  let taskIdx = parseInt(config.taskId.split('-')[1]);
  taskIdx = isGenTask? fmtTaskIdx(taskIdx + nLearnTasks): fmtTaskIdx(taskIdx);

  let box = createCustomElement("div", "box", `box-${config.taskId}`);
  box.append(createText('h1', `${taskIdx}/${nLearnTasks+nGenTasks}`));

  let taskBox = createCustomElement("div", "task-box", `taskbox-${config.taskId}`);
  const displayBox = createCustomElement("div", "display-box", `${config.taskId}-display-box`);
  displayBox.append(createDivWithStyle("stone", `${config.taskId}-agent`, config.agent));
  displayBox.append(createDivWithStyle("stone", `${config.taskId}-recipient`, config.recipient));

  const buttonGroup = createCustomElement("div", "button-group", `${config.taskId}-button-group`);
  isGenTask? buttonGroup.append(createBtn(`${config.taskId}-check-btn`, "Check", false)): null;
  buttonGroup.append(createBtn(`${config.taskId}-next-btn`, "Next", false));

  if (isGenTask) {
    const recordPan = createCustomElement("div", "record-pan", `${config.taskId}-record-pan`);
    recordPan.append(createPanel(config));
    taskBox.append(displayBox);
    taskBox.append(recordPan);
    taskBox.append(buttonGroup);
  } else {
    taskBox.append(displayBox);
    taskBox.append(buttonGroup);
  }

  box.append(taskBox);
  box.append(createTextInputPanel(config, (mode === "dev")? "flex": "none"));


  document.body.append(box);
  box.style.display = display;

  /** Button functionalities */
  const inputNextBtn = document.getElementById(`${config.taskId}-input-next-btn`);
  inputNextBtn.onclick= () => {
    const taskCount = parseInt(config.taskId.split("-")[1]);

    if (!isGenTask) {
      if(taskCount < nLearnTasks) {
        showNext(`box-learn-${fmtTaskIdx(taskCount+1)}`)
      } else {
        showNext(`box-gen-01`)
      }
    } else {
      if(taskCount < nGenTasks) {
        console.log("hello")
        showNext(`box-gen-${fmtTaskIdx(taskCount+1)}`)
      } else {
        console.log("Last")
      }
    }
  }

}

function fmtTaskIdx (counter) {
  return(counter.toString().padStart(2, '0'))
}
function getTaskConfigs(counter = 1, type = "learn") {
  let configs = {};

  configs["taskId"] = type + "-" + fmtTaskIdx(counter);
  configs["agent"] = sampleStone();
  configs["recipient"] = sampleStone();
  configs["result"] = setRules(configs["agent"], configs["recipient"])

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
 * Agent has pattern: flip recipient pattern or color (dark - light);
 * Agent has no pattern: nothing changes.
 */
function setRules (agent, recipient) {
  let result = []
  const agts = agent.split("-");
  const rcps = recipient.split("-");

  const agentPattern = agts.shift();
  const recipientPattern = rcps.shift();

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
  return result.join("-");
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
  const displayBox = createCustomElement("div", "input-box", `${config.taskId}-input-box`);
  displayBox.append(createInputForm(config));

  const buttonGroup = createCustomElement("div", "button-group", `${config.taskId}-button-group`);
  buttonGroup.append(createBtn(`${config.taskId}-input-next-btn`, "Next", (mode === "dev")? true: false));

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
  const isTraining = config.taskId.split('-')[0] === "learn";
  const selected = selection.split("-").slice(2,).join("-");
  const pass = matchSelections(config.result, selected);

  console.log(pass);

  // const passTextDiv = document.getElementById(`${config.taskId}-${pass}-text`);
  // passTextDiv.style.display = "block";

  // if (pass || (!pass && !isTraining)) {
  //   document.getElementById(`${config.taskId}-next-btn`).disabled = false;
  // } else {
  //   document.getElementById(`${config.taskId}-check-btn`).disabled = true;
  // }

  // if (isTraining || (!isTraining && pass)) {
  //   setTimeout(() => passTextDiv.style.display = "none", 3000)
  // }
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

function showNext(nextId) {
  let nextDiv = document.getElementById(nextId);
  nextDiv.style.display = "flex";
  nextDiv.scrollIntoView(true);
}
