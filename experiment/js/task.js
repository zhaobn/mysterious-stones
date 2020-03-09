

let demo = {
  "taskId": "demo",
  "agent": "rt-orange-blue",
  "recipient": "lt-yellow-orange",
  "result": "plain-blue"
}

let task01= {
  "taskId": "task01",
  "agent": "ht-blue-red",
  "recipient": "vt-red-blue"
}

function createDemo (config) {
  const taskBox = createDivWithClassId("task-box", config.taskId);

  const displayBox = createDivWithClassId("display-box", `${config.taskId}-display-box`);
  displayBox.append(createDivWithStyle("stone", `${config.taskId}-agent`, config.agent));
  displayBox.append(createDivWithStyle("stone", `${config.taskId}-recipient`, config.recipient));

  const buttonGroup = createDivWithClassId("button-group", `${config.taskId}-button-group`);
  buttonGroup.append(createBtn(`${config.taskId}-play-btn`, "Play"));
  buttonGroup.append(createBtn(`${config.taskId}-reset-btn`, "Reset", false));

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

document.body.append(createDemo(demo));
document.body.append(createTaskBox(task01));
document.getElementById(`${demo.taskId}-play-btn`).onclick = () => {
  playEffects(demo);
  setTimeout(() => {
    document.getElementById(`${demo.taskId}-reset-btn`).disabled = false;
  }, 2000);
};
document.getElementById(`${demo.taskId}-reset-btn`).onclick = () => {
  resetStones(demo);
};

// Magic effects
function playEffects (config) {
  moveStone(config);
  changeStone(config);
}

function moveStone (config) {
  const agent = `${config.taskId}-agent`;
  const recipent = `${config.taskId}-recipient`;

  const agentStone = document.getElementById(agent);
  const startPos = getCurrentLocation(agent).right;
  const endPos = getCurrentLocation(recipent).left;

  const delta = Math.round(endPos - startPos);
  (delta > 0) && (agentStone.style.left = `${delta}px`);
}

function changeStone (config) {
  const recipentStone = document.getElementById(`${config.taskId}-recipient`);
  setTimeout(() => {
    setStyle(recipentStone, config.result);
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

function setStyle (el, styleStr) {
  const fill = styleStr.split('-')[0];
  const color1 = styleStr.split('-')[1];
  const color2 = styleStr.split('-').length > 2 ? styleStr.split('-')[2] : '';

  switch (fill) {
    case "plain":
      el.style.background = color1;
      break;
    case "lt":
      el.style.background = `repeating-linear-gradient(
        -45deg, ${color1}, ${color1} 10px, ${color2} 10px, ${color2} 20px
      )`;
      break;
    case "rt":
      el.style.background = `repeating-linear-gradient(
        45deg, ${color1}, ${color1} 10px, ${color2} 10px, ${color2} 20px
      )`;
      break;
    case "ht":
      el.style.background = `repeating-linear-gradient(
        0deg, ${color1}, ${color1} 10px, ${color2} 10px, ${color2} 20px
      )`;
      break;
    case "vt":
      el.style.background = `repeating-linear-gradient(
        90deg, ${color1}, ${color1} 10px, ${color2} 10px, ${color2} 20px
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

function resetStones (config) {
  let stones = [ "agent", "recipient" ].map(s => `${config.taskId}-${s}`);
  clearElements(stones);
  createStones(config);
  document.getElementById(`${demo.taskId}-reset-btn`).disabled = true;
}

function createStones (config, box = ".display-box") {
  let el = document.querySelector(box);
  el.append(createDivWithStyle("stone", `${config.taskId}-agent`, config.agent));
  el.append(createDivWithStyle("stone", `${config.taskId}-recipient`, config.recipient));
  return(el)
}

function clearElements (els) {
  els.forEach (el => {
      let clear = document.getElementById(el);
      clear.parentNode.removeChild(clear);
  })
}
