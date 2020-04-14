
let mode = '';
let cond = "test";

/** Global variables */
let data = {};
let inputData = {};
let genData = { "taskId": [], "shape": [], "color": [], "conf": [] };
let feedbackData = {};

const svgElements = [ "svg", "polygon", "circle", "rect", "path" ];
const borderWidth = "5px";
const mar = 5;
const len = 60;

/** Configurations */
const colorDict = {
  "very_dark": "#052e54",
  "dark": "#1155cc",
  "medium": "#6d9eeb",
  "light": "#c9daf8",
  "red": "red",
  "green": "green",
}
const allColors = Object.keys(colorDict);
const allShapes = [
  "s_0", // circle,
  "p_3", // triangular
  "p_4", // square
  "p_5", // polygon 5 sides
  "p_6", // polygon 6 sides
  "p_7", // 7 sides
  "p_8", // 8 sides
  "s_s", // star
  "s_d", // donut
]
const taskConfigs = {
  "test": {
    "learn": [
      [ "rd", "g0", "gs" ],
    ],
    "gen": [
      [ "l3", "d4" ],
      [ "d5", "m3" ],
      [ "vd", "ls" ],
    ]
  },
  "A1": {
    "learn": [
      [ "l3", "m4", "d4" ],
      [ "l3", "m3", "d4" ],
      [ "l3", "l5", "m4" ],
      [ "l3", "d3", "v4" ],
    ],
    "gen": [
      [ "l3", "d4" ],
      [ "d3", "l7" ],
      [ "m6", "m3" ],
    ]
  },
  "A2": {
    "learn": [
      [ "l3", "m4", "d4" ],
      [ "d6", "m4", "d7" ],
      [ "v5", "m4", "d6" ],
      [ "l4", "m4", "d5" ],
    ],
    "gen": [
      [ "l3", "m4" ],
      [ "d3", "l4" ],
      [ "m6", "m6" ],
    ]
  },
  "B1": {
    "learn": [
      [ "l3", "l4", "l4" ],
      [ "l3", "m4", "l4" ],
      [ "l3", "d6", "l6" ],
      [ "d3", "l3", "l3" ],
      [ "d4", "m6", "d6" ],
      [ "d5", "d4", "d4" ],
    ],
    "gen": [
      [ "m3", "d4" ],
      [ "m4", "l7" ],
      [ "m5", "m3" ],
    ]
  },
  "B2": {
    "learn": [
      [ "l4", "l3", "l4" ],
      [ "m4", "l3", "l4" ],
      [ "d6", "l3", "l6" ],
      [ "l3", "d3", "l3" ],
      [ "m6", "d4", "d6" ],
      [ "d4", "d5", "d4" ],
    ],
    "gen": [
      [ "m3", "d4" ],
      [ "m4", "l7" ],
      [ "m5", "m3" ],
    ]
  },
  "C1": {
    "learn": [
      [ "l3", "m4", "m3" ],
      [ "l6", "m3", "m6" ],
      [ "l7", "m5", "m7" ],
      [ "d3", "l3", "l3" ],
      [ "d4", "l6", "l4" ],
      [ "d5", "l4", "l5" ],
    ],
    "gen": [
      [ "r0", "d4" ],
      [ "r4", "md" ],
      [ "gs", "r3" ],
    ]
  },
  "C2": {
    "learn": [
      [ "l3", "m4", "m7" ],
      [ "l6", "m3", "d6" ],
      [ "l7", "m5", "m3" ],
      [ "d3", "l3", "l4" ],
      [ "d4", "l6", "d3" ],
      [ "d5", "l4", "d6" ],
    ],
    "gen": [
      [ "r0", "d4" ],
      [ "r4", "md" ],
      [ "gs", "r3" ],
    ]
  },
}
const allStones = getAllStones(allColors, allShapes);

const learnTaskConfigs = getTaskConfigs(taskConfigs[cond].learn);
const nLearnTasks = Object.keys(learnTaskConfigs).length;

const genTaskConfigs = getTaskConfigs(taskConfigs[cond].gen);
const nGenTasks = Object.keys(genTaskConfigs).length;

/** Main body */
if (mode !== "dev") {
  document.body.append(createCustomElement("div", "section-page", "show-learning-phase"));
  document.getElementById("show-learning-phase").append(createText("h1", "Investigation starts"));

  document.body.append(createCustomElement("div", "section-page", "show-gen-phase"));
  document.getElementById("show-gen-phase").append(createText("h1", "With newly-discovered stones"));
  document.getElementById("show-gen-phase").style.display = "none";
}

for(let i = 0; i < nLearnTasks; i++ ) createLearnTaskBox(learnTaskConfigs[i], (mode === "dev")? "flex" : "none");
createTextInputPanel("initial", "none");

for(let i = 0; i < nGenTasks; i++ ) createGenTaskBox(genTaskConfigs[i], (mode === "dev")? "flex" : "none");
createTextInputPanel("final", "none");
createDebriefPage();

if (mode !== "dev") {
  setTimeout(() => {
    document.getElementById("show-learning-phase").style.display = "none";
    document.getElementById("box-learn-01").style.display = "flex";
  }, 2000);
}

/** Functions */
function createInitStones(config, parentDiv) {
  parentDiv.append(createStone("new-stone", `${config.taskId}-agent`, getOpts(config.agent, true)));
  parentDiv.append(createStone("new-stone", `${config.taskId}-recipient`, getOpts(config.recipient, false)));
  return(parentDiv);
}
function createStones (config) {
  let el = document.getElementById(`${config.taskId}-display-box`);
  el.append(createStone("new-stone", `${config.taskId}-agent`, getOpts(config.agent, true)));
  el.append(createStone("new-stone", `${config.taskId}-recipient`, getOpts(config.recipient, false)));
  return(el)
}
function getOpts (style, isAgent) {
  const color = style.split(";")[0];
  const shape = style.split(";")[1];
  let opts = {};
  opts["color"] = colorDict[color];
  opts["hasBorder"] = isAgent;
  if (shape[0] === "p") {
    const n = shape.split("_")[1];
    opts["points"] = calcPolygon({n:n,r:len/2,a:0});
  } else {
    switch (shape[2]) {
      case "0":
        opts["cx"] = len/2;
        opts["cy"] = len/2;
        opts["r"] = len/2-mar;
        break;
      case "s":
        opts["points"] = calcStar();
        opts["transform"] = "rotate(55deg)";
        break;
      case "d":
        opts["d"] = calcDonut();
        break;

    }

  }
  return opts;
}
function createAnswerComposer(config) {
  const taskId = config.taskId;
  let box = createCustomElement("div", "display-box", `${taskId}-selection-box`);
  box.style.width = "40%";

  const extraColors = (cond[0] === "C")? `
    <option value="red">Red</option>
    <option value="green">Green</option>` : '';
  const extraShapes = (cond[0] === "C")? `
    <option value="s_0">Circle</option>
    <option value="s_d">Donut</option>
    <option value="s_s">Star</option>` : '';
  box.innerHTML = `
    <div class="selection-composer">
      <div class="selection-form-div">
        <form class="selection-form" id="${taskId}-selection-form">
          <p>Shape:
          <select id="shape" name="shape" class="selection-input">
            <option value="--" SELECTED>--</option>
            <option value="p_3">Triangle</option>
            <option value="p_4">Square</option>
            <option value="p_5">Pentagon</option>
            <option value="p_6">Hexagon</option>
            <option value="p_7">Heptagon</option>
            ${extraShapes}
          </select>
          </p>
          <p>Shading:
          <select id="color" name="color" class="selection-input">
            <option value="--" SELECTED>--</option>
            <option value="light">Light</option>
            <option value="medium">Medium</option>
            <option value="dark">Dark</option>
            <option value="very_dark">Very dark</option>
            ${extraColors}
          </select>
          <p>Your confidence:
          <select id="conf" name="conf" class="selection-input">
            <option value="--" SELECTED>--</option>
            <option value="10">10 - Very confident</option>
            <option value="9">9</option>
            <option value="8">8</option>
            <option value="7">7</option>
            <option value="6">6</option>
            <option value="5">5 - Moderately confident</option>
            <option value="4">4</option>
            <option value="3">3</option>
            <option value="2">2</option>
            <option value="1">1 - Not confident at all</option>
          </select>
          </p>
        </form>
      </div>
      <div class="selection-svg-div">
        <svg class="selection-object" id='${taskId}-selection-svg'></svg>
      </div>
      <div class="selection-buttons">
        <button class="task-button" id="${taskId}-confirm-btn" disabled>Confirm</button>
      </div>
    </div>`
  return box;
}

function fmtTaskIdx (counter) {
  return(counter.toString().padStart(2, '0'))
}
function sampleTasks (type, count) {
  let tasks = [];
  for(let i = 1; i <= count; i++) {
    taskConfig = {};
    taskConfig["taskId"] = type+"-"+fmtTaskIdx(i);
    taskConfig["type"] = type;
    taskConfig["index"] = i;
    taskConfig["agent"] = sampleObj(allStones);
    taskConfig["recipient"] = sampleObj(allStones);
    taskConfig["result"] = sampleObj(allStones);
    tasks.push(taskConfig);
  }
  return tasks;
}
function getAllStones (colors, shapes) {
  let stones = []
  colors.forEach(c => {
    shapes.forEach(s => stones.push(c + ';' + s))
  })
  return(stones);
}
function sampleObj (objs) {
  return(objs[Math.floor(Math.random() * objs.length)]);
}
function createBtn (btnId, text = "Button", on = true, className = "task-button") {
  let btn = createCustomElement("button", className, btnId);
  btn.disabled = !on;
  (text.length > 0) ? btn.append(document.createTextNode(text)): null;
  return(btn)
}
function attachStone (svg, id, opts, shapeClass = 'shape') {
  if (Object.keys(opts).indexOf("points") < 0) {
    if (Object.keys(opts).indexOf("d") < 0) {
      svg.append(createCircle(shapeClass, `${id}`, opts));
    } else {
      svg.append(createDonut(shapeClass, `${id}`, opts));
    }
  } else {
    svg.append(createPolygon(shapeClass, `${id}`, opts))
  }
  return svg
}
function createStone (stoneClass, id, opts, svgClass = 'test', shapeClass = 'shape') {
  let div = createCustomElement("div", stoneClass, id);
  let svg = createCustomElement("svg", svgClass, `${id}-svg`);
  svg = attachStone(svg, `${id}-stone`, opts);
  div.append(svg)
  return(div);
}
function createDonut (className, id, opts) {
  let donut = createCustomElement("path", className, id);
  setAttributes(donut, {
    "fill": opts.color,
    "d": opts.d,
    "stroke-width": opts.hasBorder? borderWidth : "0px",
  })
  return(donut);
}
function createPolygon(className, id, opts) {
  let polygon = createCustomElement("polygon", className, id);
  setAttributes(polygon, {
    "fill": opts.color,
    "points": opts.points,
    "stroke-width": opts.hasBorder? borderWidth : "0px",
  });
  return(polygon);
}
function createCircle (className, id, opts) {
  let circle = createCustomElement("circle", className, id);
  setAttributes(circle, {
    "cx": opts.cx,
    "cy": opts.cy,
    "r": opts.r,
    "fill": opts.color,
    "stroke-width": opts.hasBorder? borderWidth : "0px",
  })
  return(circle);
}
function calcPolygon(input) {
  let output = [];
  if (input.n === "3") {
    output.push(`${len/2},${mar}`);
    output.push(`${len-mar},${len-mar}`);
    output.push(`${mar},${len-mar}`);
  } else {
    // Adapted from https://gist.github.com/jonthesquirrel/e2807811d58a6627ded4
    for (let i = 1; i <= input.n; i++) {
      output.push(
        ((input.r * Math.cos(input.a + 2 * i * Math.PI / input.n)) + len/2).toFixed(0).toString() + "," +
        ((input.r * Math.sin(input.a + 2 * i * Math.PI / input.n)) + len/2).toFixed(0).toString()
      )
    }
  }
  return output.join(" ")
}
function createCustomElement (type = 'div', className, id) {
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
function createTextInputPanel (taskId, display = "none") {
  let box = createCustomElement("div", "box", `box-${taskId}-input`);
  let taskBox = createCustomElement("div", "input-div", `${taskId}-input`);

  const displayBox = createCustomElement("div", "input-box", `${taskId}-input-box`);
  displayBox.append(createInputForm(taskId));

  const buttonGroup = createCustomElement("div", "button-group", `${taskId}-button-group`);
  buttonGroup.append(createBtn(`${taskId}-input-submit-btn`, "Submit", false));
  buttonGroup.append(createBtn(`${taskId}-input-next-btn`, "Next",
    (mode === "dev" || mode === "debug")? true: false));

  taskBox.append(displayBox);
  taskBox.append(buttonGroup);

  box.append(taskBox);
  document.body.append(box);
  box.style.display = (mode === "dev")? "flex": display;

  /** Button functionalities */
  const submitBtn = document.getElementById(`${taskId}-input-submit-btn`);
  const inputNextBtn = document.getElementById(`${taskId}-input-next-btn`);
  const inputForm = document.getElementById(`${taskId}-input-form`);

  inputForm.onchange = () => isFilled(`${taskId}-input-form`)? submitBtn.disabled = false: null;
  submitBtn.onclick = () => {
    let inputs = inputForm.elements;
    Object.keys(inputs).forEach(id => saveFormData(inputs[id], inputData));
    data["inputs"] = inputData;
    submitBtn.disabled = true;
    disableFormInputs(`${taskId}-input-form`);
    inputNextBtn.disabled = false;
  }
  inputNextBtn.onclick = () => {
    if (taskId === "initial") {
      hide("box-initial-input");
      showNext("box-gen-01");
    } else if (taskId === "final") {
      for (let i = 0; i < nLearnTasks; i++) hide(`box-learn-${fmtTaskIdx(i + 1)}`)
      for (let i = 0;  i < nGenTasks; i++) hide(`box-gen-${fmtTaskIdx(i + 1)}`)
      hide("box-final-input");
      showNext("debrief");
    } else {
      console.log("taskId not found!")
    }
  }
}
function createInputForm(taskId) {
  let form = createCustomElement("form", "input-form", `${taskId}-input-form`);
  const placeholderText = `Please type here`
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
    <p>
      <b>What is your ${taskId} impression about how these mysterious stones work?</b>
      (Please refer to stones as <i>active</i> and <i>inactive</i>,
      and be specific about <i>what properties you think matter or do not matter for the effects,
      and how they do so</i>.)
    </p>
    <textarea name="${taskId}-input" id="${taskId}-input" placeholder="${placeholderText}"></textarea>
    <p>How certain are you?
      <select id="${taskId}-input-certainty" name="${taskId}-input-certainty" class="input-rule">
        ${options}
      </select>
    </p>
    `
  return(form);
}
function disableFormInputs (formId) {
  const form = document.getElementById(formId);
  const inputs = form.elements;
  (Object.keys(inputs)).forEach((input) => inputs[input].disabled = true);
}
function showNext(id, display = "flex") {
  let div = document.getElementById(id);
  div.style.display = display;
  div.scrollIntoView(mode === 'dev'? false: true);
}
function hide(id) {
  let div = document.getElementById(id);
  div.style.display = "none";
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
function playEffects (config) {
  const getCurrentLocation = (id) => {
    let rect = {top: 0, bottom: 0, left: 0, right: 0};
    const pos = document.getElementById(id).getBoundingClientRect();
    rect.top = pos.top;
    rect.bottom = pos.bottom;
    rect.left = pos.left;
    rect.right = pos.right;
    return rect;
  }
  if (!(document.body.contains(document.getElementById(`${config.taskId}-agent`)))) {
    createStones(config)
  }
  const agent = `${config.taskId}-agent`;
  const recipient = `${config.taskId}-recipient`;

  const agentStone = document.getElementById(agent);
  const startPos = getCurrentLocation(agent).right;
  const endPos = getCurrentLocation(recipient).left;

  const delta = Math.round(endPos - startPos) + 15;
  (delta > 0) && (agentStone.style.left = `${delta}px`);

  setTimeout(() => {
    let svg = document.getElementById(`${config.taskId}-recipient-svg`);
    clearElement(`${config.taskId}-recipient-stone`);
    svg = attachStone(svg, `${config.taskId}-result-stone`, getOpts(config.result, false));
  }, 1500);
}
function clearStones (config) {
  let els = [ "agent", "recipient" ].map(s => `${config.taskId}-${s}`);
  els.forEach (el => clearElement(el));
}
function clearElement (id) {
  let clear = document.getElementById(id);
  if(!(clear === null)) clear.parentNode.removeChild(clear);
}
function createSummaryStones(config, parentDiv, stoneClass = "new-stone") {
  createSumBox = (sumBox, type) => {
    let textDiv = createCustomElement("div", "sum-text", `${config.taskId}-sumbox-${type}-text`);
    textDiv.append(createText("h2", capFirstLetter(type)));
    let sumDiv = createCustomElement("div", "sum-display", `${config.taskId}-sumbox-${type}-display`);
    if (type === "before") {
      sumDiv.append(createStone(stoneClass, `${config.taskId}-agent`, getOpts(config.agent, true)));
      sumDiv.append(createStone(stoneClass, `${config.taskId}-recipient`, getOpts(config.recipient, false)));
      sumDiv.style.justifyContent = "space-between";
    } else if (type === "after") {
      sumDiv.append(createStone(stoneClass, `${config.taskId}-agent`, getOpts(config.agent, true)));
      sumDiv.append(createStone(stoneClass, `${config.taskId}-result`, getOpts(config.result, false)));
      sumDiv.style.justifyContent = "flex-end";
    } else {
      console.log("Summary type not match @createSummaryStones()")
    }
    sumBox.append(textDiv);
    sumBox.append(sumDiv);
    return sumBox;
  }
  let beforeBox = createCustomElement("div", "sum-box", `${config.taskId}-sumbox-before`);
  let afterBox = createCustomElement("div", "sum-box", `${config.taskId}-sumbox-after`);

  beforeBox = createSumBox(beforeBox, "before");
  afterBox = createSumBox(afterBox, "after");
  parentDiv.append(beforeBox);
  parentDiv.append(afterBox);
  return parentDiv;
}
function capFirstLetter (str) {
  let fl = str[0].toUpperCase();
  return(fl + str.slice(1,));
}
function currentSelection (formId) {
  let selections = [];
  const inputs = document.getElementById(formId).elements;
  (Object.keys(inputs)).forEach((input) => {
    selections.push(inputs[input].value)
  });
  return selections.reverse().join(";")
}
function saveFormData (input, dataObj) {
  let fieldName = input.name;
  dataObj[fieldName] = input.value;
  return dataObj;
}

/** Auto-download data file for off-line use */
function download(content, fileName, contentType) {
  var a = document.createElement("a");
  var file = new Blob([content], {type: contentType});
  a.href = URL.createObjectURL(file);
  a.download = fileName;
  a.click();
}

function saveData (dataFile) {
  console.log(dataFile);
  download(JSON.stringify(dataFile), 'data.txt', '"text/csv"');
}

function composeSelection (svgid, formid, checkBtnId) {
  const selections = currentSelection(formid).split(";");
  const confidence = selections[0];
  const color = selections[1];
  const shape = selections[2];

  if (!(color === "--" || shape === "--")) {
    let svg = document.getElementById(svgid);
    if (svg.childNodes.length > 0) {
      clearElement("test-stone")
    };
    svg = attachStone(svg, "test-stone", getOpts(color+";"+shape));
  }
  let checkBtn = document.getElementById(checkBtnId);
  if (!(color === '--' || shape === '--' || confidence === '--')) checkBtn.disabled = false;
}
function getTaskConfigs (settings) {
  const taskType = (settings[0].length > 2)? "learn" : "gen";
  const readStone = (str) => {
    let shape = ((parseInt(str[1]) > 0)? "p" : "s") + '_' + str[1];
    let color = "";
    switch (str[0]) {
      case "l":
        color = "light";
        break;
      case "m":
        color = "medium";
        break;
      case "d":
        color = "dark";
        break;
      case "v":
        color = "very_dark";
        break;
      case "r":
        color = "red";
        break;
      case "g":
        color = "green";
        break;
    }
    return color + ";" + shape;
  }
  const configureTask = (setting, index) => {
    let task = {};
    task["taskId"] = taskType + "-" + fmtTaskIdx(index);
    task["type"] = taskType;
    task["index"] = index;
    task["agent"] = readStone(setting[0]);
    task["recipient"] = readStone(setting[1]);
    (taskType === "learn")? task["result"] = readStone(setting[2]) : null;
    return task;
  }
  let configs = [];
  settings.forEach((s, i) => configs.push(configureTask(s, i + 1)));

  return configs;
}
function createLearnTaskBox (config, display = "none") {
  const taskType = config.type;
  const taskId = config.taskId;

  let index = config.index;
  const total = (taskType === "learn")? nLearnTasks : nGenTasks;

  let box = createCustomElement("div", "box", `box-${taskId}`);
  let taskNum = createCustomElement("div", "task-num", `${taskId}-num`);
  taskNum.append(createText('h1', index + '/' + total));

  let taskBox = createCustomElement("div", "task-box", `taskbox-${taskId}`);
  taskBox.append(taskNum);
  let displayBox = createCustomElement("div", "display-box", `${taskId}-display-box`);
  displayBox = createInitStones(config, displayBox);

  const buttonGroup = createCustomElement("div", "button-group", `${taskId}-button-group`);
  buttonGroup.append(createBtn(`${taskId}-play-btn`, "Play", true));
  buttonGroup.append(createBtn(`${taskId}-next-btn`, "Next", false));

  taskBox.append(displayBox);
  taskBox.append(buttonGroup);

  box.append(taskBox);

  document.body.append(box);
  box.style.display = display;

  // /** Button functionalities */
  const playBtn = document.getElementById(`${taskId}-play-btn`);
  const nextBtn = document.getElementById(`${taskId}-next-btn`);

  playBtn.onclick = () => {
    playBtn.disabled = true;
    playEffects(config);
    setTimeout(() => {
      clearStones(config);
      setTimeout(() => {
        displayBox = createSummaryStones(config, displayBox);
        nextBtn.disabled = false;
      }, 1000);
    }, 3500);
  }
  nextBtn.onclick = () => {
    nextBtn.disabled = true;
    document.getElementById(`taskbox-${taskId}`).style.height = '200px';
    document.getElementById(`${taskId}-display-box`).style.backgroundColor = '#e0e0e0';
    if (index < nLearnTasks) {
      showNext(`box-${taskType}-${fmtTaskIdx(index + 1)}`);
    } else {
      showNext("box-initial-input");
    }
  }
}

function createGenTaskBox (config, display = "none") {
  const taskId = config.taskId;

  let index = config.index;

  let box = createCustomElement("div", "box", `box-${taskId}`);
  let taskNum = createCustomElement("div", "task-num", `${taskId}-num`);
  taskNum.append(createText('h1', index + '/' + nGenTasks));

  let textDiv = createCustomElement("div", "text-div", `${taskId}-text-div`);
  textDiv.append(createText("h1", "This agent will turn this recipient into ...?"));

  let taskDiv = createCustomElement("div", "task-div", `${taskId}-task-div`);
  let displayDiv = createCustomElement("div", "display-div", `${taskId}-display-div`);

  let taskBox = createCustomElement("div", "task-box", `taskbox-${taskId}`);
  taskBox.append(taskNum);
  let displayBox = createCustomElement("div", "display-box", `${taskId}-display-box`);
  displayBox = createInitStones(config, displayBox);

  displayDiv.append(displayBox);
  displayDiv.append(createAnswerComposer(config));

  taskDiv.append(textDiv);
  taskDiv.append(displayDiv);

  taskBox.append(taskNum);
  taskBox.append(taskDiv);

  box.append(taskBox);

  document.body.append(box);
  box.style.display = display;

  /** Interactivities */
  const selectionForm = document.getElementById(`${taskId}-selection-form`);
  const confirmBtn = document.getElementById(`${taskId}-confirm-btn`);
  selectionForm.onchange = () =>
    composeSelection(`${taskId}-selection-svg`, `${taskId}-selection-form`, `${taskId}-confirm-btn`);
  confirmBtn.onclick = () => {
    genData["taskId"].push(taskId);
    let inputs = selectionForm.elements;
    Object.keys(inputs).forEach(id => genData[inputs[id].name].push(inputs[id].value));
    data["gen"] = genData;
    disableFormInputs(`${taskId}-selection-form`);
    if (index < nGenTasks) {
      hide(`box-${taskId}`)
      showNext(`box-gen-${fmtTaskIdx(index + 1)}`)
    } else {
      showNext("box-final-input")
    }
  }
}
// function createHistory (configs) {
//   const createSummary = (config) => {
//     config.taskId = "sum" + config.taskId;
//     let div = createCustomElement("div", "hist-box", `ssum-${config.taskId}`);
//     let taskNum = createCustomElement("div", "task-num", `ssum-${config.taskId}-task-num`);
//     taskNum.append(createText("h1", config.index));
//     let sumDiv = createCustomElement("div","sum-div", `ssum-${config.taskId}-sum-div`);
//     sumDiv = createSummaryStones(config, sumDiv);
//     div.append(taskNum);
//     div.append(sumDiv);
//     return(div)
//   }
//   let box = createCustomElement("div", "box", "box-history");
//   let textDiv = createCustomElement("div", "box", "box-history");
//   textDiv.append(createText("h1", "Summary"));

//   box.append(textDiv);

//   configs.forEach(c => box.append(createSummary(c)));

//   document.body.append(box)
// }
function createDebriefPage (display = "none") {
  const debrief = createCustomElement("div", "box", "debrief");
  debrief.innerHTML = `
  <h1>Thank you for your contributions to science</h1>
    <h4>You will be eligible for full payment once you answer the following questions.</h4>
    <h4><b>Note</b>: To continue, please answer <span style="color:red">all questions</span>.</h4>
    <form id="postquiz"  class='frame' action="debrief" method="post">
      <b>1.&nbsp;</b>How old are you?:
       <input
          id="age" name="age" class="posttestQ" type="number"
          maxlength = "3" min="18" max="100"step = "1"
        > years
      </p>
      <p>
        <b>2.&nbsp;</b>What is your gender?
        <select id="sex" name="sex" class="posttestQ">
          <option value="noresp" SELECTED></option>
          <option value="female">Female</option>
          <option value="male">Male</option>
          <option value="other">Other</option>
          <option value="noresponse">I’d prefer not to respond</option>
        </select>
      </p>
      <p>
        <b>3.&nbsp;</b>On a scale of 1-10 (where 10 is the most engaged), please rate how <b>ENGAGING</b> you found the learning task:
        <select id="engagement" name="engagement" class="posttestQ">
          <option value="--" SELECTED>
          <option value="10">10 - Very engaging</option>
          <option value="9">9</option>
          <option value="8">8</option>
          <option value="7">7</option>
          <option value="6">6</option>
          <option value="5">5 - Moderately</option>
          <option value="4">4</option>
          <option value="3">3</option>
          <option value="2">2</option>
          <option value="1">1</option>
          <option value="0">0 - Not engaged</option>
        </select>
      </p>
      <p>
        <b>4.&nbsp;</b>On a scale of 1-10 (where 10 is the most difficult), please rate how <b>DIFFICULT</b> you found the learning task:
        <select id="difficulty" name="difficulty" class="posttestQ">
          <option value="--" SELECTED>
          <option value="10">10 - Very difficult</option>
          <option value="9">9</option>
          <option value="8">8</option>
          <option value="7">7</option>
          <option value="6">6</option>
          <option value="5">5 - Moderately difficult</option>
          <option value="4">4</option>
          <option value="3">3</option>
          <option value="2">2</option>
          <option value="1">1</option>
          <option value="0">0 - Not difficult at all</option>
        </select>
      </p>
      <p><b>5.&nbsp;</b>Do you have any comments regarding the experiment?</p>
      <textarea name="feedback" id="feedback" placeholder="Please type here"></textarea>
    </form>
    <div class='button-group-vc'>
      <button class='big-button' id='done-btn' disabled>Done</button>
    </div>
  `
  debrief.style.display = display;
  document.body.append(debrief);

  const doneBtn = document.getElementById('done-btn');
  const debriefForm = document.getElementById('postquiz');

  debriefForm.onchange = () => {
    isFilled('postquiz')? doneBtn.disabled = false: null;
  }
  doneBtn.onclick = () => {
    let inputs = debriefForm.elements;
    Object.keys(inputs).forEach(id => saveFormData(inputs[id], feedbackData));
    data["feedback"] = feedbackData;
    saveData(data);
  };
}
function calcDonut(outercx = len/2, outercy = len/2, outerr = len/2-mar, innercx = len/2, innercy = len/2, innerr = len/4-mar) {
  // http://xn--dahlstrm-t4a.net/svg/path/donut-shop.html
  return "M" + outercx + " " + outercy + "m-" + outerr + ",0a" + outerr + "," + outerr + ",0 1,0 " + (outerr * 2) + ",0a " + outerr + "," + outerr + " 0 1,0 -" + (outerr * 2) + ",0z" +
       "M" + innercx + " " + innercy + "m-" + innerr + ",0a" + innerr + "," + innerr + ",0 0,1 " + (innerr * 2) + ",0a " + innerr + "," + innerr + " 0 0,1 -" + (innerr * 2) + ",0z";
}
function calcStar(arms = 5, centerX = len/2, centerY = len/2, outerRadius = len/2, innerRadius = len/4){
  //https://dillieodigital.wordpress.com/2013/01/16/quick-tip-how-to-draw-a-star-with-svg-and-javascript/
  let results = "";
  let angle = Math.PI / arms;
  for (let i = 0; i < 2 * arms; i++) {
    let r = (i & 1) == 0 ? outerRadius : innerRadius;
    let currX = Math.round(centerX + Math.cos(i * angle) * r);
    let currY = Math.round(centerY + Math.sin(i * angle) * r);
    results = (i == 0)? currX + "," + currY : results + ", " + currX + "," + currY
  }
   return results;
}
