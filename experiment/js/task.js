
let mode = 'debug';
let cond = "A1";

/** Global variables */
const svgElements = [ "svg", "polygon", "circle", "rect" ];
const borderWidth = "5px";
const mar = 5;
const len = 60;
let textSelection = "";

/** Configurations */
const colorDict = {
  "very_dark": "#1c4587",
  "dark": "#1155cc",
  "medium": "#6d9eeb",
  "light": "#c9daf8",
}
const allColors = Object.keys(colorDict);
const allShapes = [
  "circle",
  "p_3", // triangular
  "p_4", // square
  "p_5", // polygon 5 sides
  "p_6", // polygon 6 sides
  "p_7", // 7 sides
  "p_8", // 8 sides
]

const taskConfigs = {
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
  }
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

  document.body.append(createCustomElement("div", "section-page", "show-test-phase"));
  document.getElementById("show-test-phase").append(createText("h1", "Tests"));
  document.getElementById("show-test-phase").style.display = "none";

  document.body.append(createCustomElement("div", "section-page", "show-gen-phase"));
  document.getElementById("show-gen-phase").append(createText("h1", "With newly-discovered stones"));
  document.getElementById("show-gen-phase").style.display = "none";
}

for(let i = 0; i < nLearnTasks; i++ ) {
  createTaskBox(learnTaskConfigs[i], (mode === "dev")? "flex" : "none");
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
    opts["points"] = calcPolygon({n:n,r:len/2,a:0})
  } else {
    opts["cx"] = len/2;
    opts["cy"] = len/2;
    opts["r"] = len/2-mar;
  }
  return opts;
}
function createAnswerComposer(config) {
  const taskId = config.taskId;
  let box = createCustomElement("div", "display-box", `${taskId}-selection-box`);
  box.style.width = "40%";

  // let composer = createCustomElement("div", "selection-composer", `${taskId}-selection-composer`);
  // let svg

  box.innerHTML = `
    <div class="selection-composer">
      <div class="selection-svg-div">
        <svg class="selection-object" id='${taskId}-selection-svg'></svg>
      </div>
      <div class="selection-form-div">
        <form class="selection-form" id="${taskId}-selection-form">
          <p>Shape</p>
          <select id="color" name="color" class="selection-input">
            <option value="--" SELECTED>--</option>
            <option value="circle">Circle</option>
            <option value="p_3">Triangle</option>
            <option value="p_4">Square</option>
            <option value="p_4">Square</option>
            <option value="p_5">Pentagon</option>
            <option value="p_6">Hexagon</option>
            <option value="p_7">Heptagon</option>
          </select>
          <p>Shading</p>
          <select id="color" name="color" class="selection-input">
            <option value="--" SELECTED>--</option>
            <option value="dark">Dark</option>
            <option value="medium">Medium</option>
            <option value="light">Light</option>
          </select>
        </form>
        <br />
        <button class="task-button" id="${taskId}-check-btn" disabled>Check</button>
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
    svg.append(createCircle(shapeClass, `${id}`, opts));
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
function createPolygon(className, id, opts) {
  let polygon = createCustomElement("polygon", className, id);
  setAttributes(polygon, {
    "fill": opts.color,
    "points": opts.points,
    "stroke-width": opts.hasBorder? borderWidth : "0px",
  })
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
function createTextInputPanel (config, display = "none") {
  const taskBox = createCustomElement("div", "input-div", `${config.taskId}-input`);
  taskBox.style.width = "100%";

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
  editBtns.append(createBtn(`${config.taskId}-paste-btn`, "Paste", (config.index === 1)? false : true));

  const displayBox = createCustomElement("div", "input-box", `${config.taskId}-input-box`);
  displayBox.append(createInputForm(config));
  displayBox.append(editBtns);

  // const buttonGroup = createCustomElement("div", "button-group", `${config.taskId}-button-group`);
  // buttonGroup.append(createBtn(`${config.taskId}-input-next-btn`, "Next",
  //   (mode === "dev" || mode === "debug")? true: false));

  // taskBox.append(instructionPan);
  taskBox.append(displayBox);
  // taskBox.append(buttonGroup);

  taskBox.style.display = display;

  return(taskBox);
}
function createInputForm(config) {
  let form = createCustomElement("form", "input-form", `${config.taskId}-input-form`);
  const placeholderText = `Please refer to objects as agent, recipient, result; please refer to properties using plain, stripes, and directions.`
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
    <p>Such effect is probably because</p>
    <textarea name="${config.taskId}-input-1" id="${config.taskId}-input-1" placeholder="${placeholderText}"></textarea>
    <p>How certain are you?
      <select id="${config.taskId}-input-1-certainty" name="${config.taskId}-input-1-certainty" class="input-rule">
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
function showNext(nextId) {
  let nextDiv = document.getElementById(nextId);
  nextDiv.style.display = "flex";
  nextDiv.scrollIntoView(mode === 'dev'? false: true);
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
function createSummaryStones(config, parentDiv) {
  createSumBox = (sumBox, type) => {
    let textDiv = createCustomElement("div", "sum-text", `${config.taskId}-sumbox-${type}-text`);
    textDiv.append(createText("h2", capFirstLetter(type)));
    let sumDiv = createCustomElement("div", "sum-display", `${config.taskId}-sumbox-${type}-display`);
    if (type === "before") {
      sumDiv.append(createStone("new-stone", `${config.taskId}-agent`, getOpts(config.agent, true)));
      sumDiv.append(createStone("new-stone", `${config.taskId}-recipient`, getOpts(config.recipient, false)));
      sumDiv.style.justifyContent = "space-between";
    } else if (type === "after") {
      sumDiv.append(createStone("new-stone", `${config.taskId}-agent`, getOpts(config.agent, true)));
      sumDiv.append(createStone("new-stone", `${config.taskId}-result`, getOpts(config.result, false)));
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
function composeSelection (svgid, formid, checkBtnId) {
  const selection = currentSelection(formid);
  const checkBtn = document.getElementById(checkBtnId);
  if (!(selection.split(";")[0] === "--" || selection.split(";")[1] === "--")) {
    checkBtn.disabled = false;
    let svg = document.getElementById(svgid);
    if (svg.childNodes.length > 0) {
      clearElement("test-stone")
    };
    svg = attachStone(svg, "test-stone", getOpts(selection));
  }
}
function getTaskConfigs (settings) {
  const taskType = (settings[0].length > 2)? "learn" : "gen";
  const readStone = (str) => {
    let color = "";
    let shape = "p_" + str[1];
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
function createTaskBox (config, display = "none") {
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

  if (taskType === "learn") {
    const buttonGroup = createCustomElement("div", "button-group", `${taskId}-button-group`);
    buttonGroup.append(createBtn(`${taskId}-play-btn`, "Play", true));
    buttonGroup.append(createBtn(`${taskId}-next-btn`, "Next", false));

    taskBox.append(displayBox);
    taskBox.append(buttonGroup);
  } else if (taskType === "gen") {
    taskBox.append(displayBox);
    taskBox.append(createAnswerComposer(config));
  } else {
    console.log("Task type not match")
  }

  box.append(taskBox);

  document.body.append(box);
  box.style.display = display;

  // /** Button functionalities */
  if (taskType === "learn") {
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
      document.getElementById(`taskbox-${taskId}`).style.height = '200px';
      nextBtn.disabled = true;
      showNext(`box-${taskType}-${fmtTaskIdx(index + 1)}`);
    }
  }
}
