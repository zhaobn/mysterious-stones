
let mode = 'dev';

/** Global variables */
const svgElements = [ "svg", "circle", "polygon", "rect" ];
const borderWidth = "8px";
const mar = 5;
const len = 60;
let textSelection = "";

/** Configurations */
const colorDict = {
  "dark_1": '#6A1B9A',
  "dark_2": '#1565C0',
  "light_1": '#AB47BC',
  "light_2": '#64B5F6',
  "dark": "navy",
  "medium": "blue",
  "light": "lightblue",
  "--": "white",
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

const allStones = getAllStones(allColors, allShapes);

const learnTaskConfigs = sampleTasks('learn', 3);
const nLearnTasks = Object.keys(learnTaskConfigs).length;

const testTaskConfigs = sampleTasks('test', 1);
const nTestTasks = Object.keys(testTaskConfigs).length;

const genTaskConfigs = sampleTasks('gen', 2);
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

// for(let i = 0; i < nLearnTasks; i++ ) {
//   createTaskBox(learnTaskConfigs[i], (mode === "dev")? "flex" : "none");
// }

// if (mode !== "dev") {
//   setTimeout(() => {
//     document.getElementById("show-learning-phase").style.display = "none";
//     document.getElementById("box-learn-01").style.display = "flex";
//   }, 2000);
// }

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

  // if (taskType !== "learn") {
  //   const feedbackPass = createCustomElement("div", "feedback-true", `${taskId}-true-text`);
  //   feedbackPass.append(document.createTextNode("Correct! See above for the effects summary."))
  //   feedbackPass.style.display = "none";

  //   const feedbackFail = createCustomElement("div", "feedback-false", `${taskId}-false-text`);
  //   feedbackFail.append(document.createTextNode("Wrong! See above for the real effects summary."));
  //   feedbackFail.style.display = "none";

  //   box.append(feedbackPass);
  //   box.append(feedbackFail);
  // }

  box.append(createTextInputPanel(config, (mode === "dev" || mode === "debug")? "flex": "none"));

  document.body.append(box);
  box.style.display = display;

  // /** Button functionalities */
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
        clearStones(config);
        setTimeout(() => {
          displayBox = createSummaryStones(config, displayBox);
          showNext(`${taskId}-input`);
        }, 1000);
      }, 3500);
    }
  }

  inputForm.onchange = () => isFilled(`${taskId}-input-form`)? inputNextBtn.disabled = false: null;

  inputNextBtn.onclick= () => {
    inputNextBtn.disabled = true;
    // (taskType === "gen")? gtData = saveFormData(config, gtData) : ltData = saveFormData(config, ltData);
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
  // Adapted from https://gist.github.com/jonthesquirrel/e2807811d58a6627ded4
  let output = [];
  for (let i = 1; i <= input.n; i++) {
    output.push(
      ((input.r * Math.cos(input.a + 2 * i * Math.PI / input.n)) + len/2).toFixed(0).toString() + "," +
      ((input.r * Math.sin(input.a + 2 * i * Math.PI / input.n)) + len/2).toFixed(0).toString()
    )
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
  editBtns.append(createBtn(`${config.taskId}-paste-btn`, "Paste", (config.index === 1)? false : true));

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
function copyText (id) {
  textSelection = document.getElementById(id).value;
}

function pasteText (id) {
  const textArea = document.getElementById(id);
  let text = textArea.value;
  text = text + " " + textSelection;
  textArea.value = text;
}
// function saveFormData(config, dataObj) {
//   const form = document.getElementById(`${config.taskId}-input-form`);
//   const inputs = form.elements;
//   (Object.keys(inputs)).forEach((input) => {
//     let field = inputs[input];
//     if (field.name.indexOf("certainty") < 0) {
//       dataObj["report"].push(field.value);
//     } else {
//       dataObj["confidence"].push(field.value);
//     }
//   })
//   return(dataObj);
// }
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
  if (!(document.body.contains(document.getElementById(`${config.taskId}-agent`)))) {
    createStones(config)
  }
  const agent = `${config.taskId}-agent`;
  const recipient = `${config.taskId}-recipient`;

  const agentStone = document.getElementById(agent);
  const startPos = getCurrentLocation(agent).right;
  const endPos = getCurrentLocation(recipient).left;

  const delta = Math.round(endPos - startPos) + 5;
  (delta > 0) && (agentStone.style.left = `${delta}px`);

  setTimeout(() => {
    let svg = document.getElementById(`${config.taskId}-recipient-svg`);
    clearElement(`${config.taskId}-recipient-stone`);
    svg = attachStone(svg, `${config.taskId}-recipient-stone`, getOpts(config.result, false));
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
      sumDiv.append(createStone("new-stone", `${config.taskId}-result`, getOpts(config.recipient, false)));
      sumDiv.style.justifyContent = "center";
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
