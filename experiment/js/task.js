

let learnDemo = {
  "taskId": "learn-demo",
  "bg": "pink-bg",
  "agent": "blue-plain",
  "recipient": "yellow-plain",
  "result": "blue-yellow-left"
}

let task01= {
  "taskId": "task01",
  "bg": "pink-bg",
  "agent": "blue-plain",
  "recipient": "yellow-plain",
  "result": "blue-yellow-left",
}

function createLearnDemo (config) {
  const taskBox = createDivWithClassId("task-box", "learn-demo");

  const displayBox = createDivWithClassId("display-box", "learn-demo-display-box");
  const learnDemoBg = createDivWithClassId(config.bg, "learn-demo-bg");
  const learnDemoAgent = createDivWithClassId(config.agent, "learn-demo-agent");
  const learnDemoRecipient = createDivWithClassId(config.recipient, "learn-demo-recipient");
  learnDemoBg.append(learnDemoAgent);
  learnDemoBg.append(learnDemoRecipient);
  displayBox.append(learnDemoBg);

  const recordPan = createDivWithClassId("record-pan", "learn-demo-record-pan");
  recordPan.append(document.createTextNode("Original recipient"));
  const learnDemoResult = createDivWithClassId(config.recipient, "learn-demo-original");
  recordPan.append(learnDemoResult);

  const buttonGroup = createDivWithClassId("button-group", "learn-demo-record-pan");
  const playButton = createDivWithClassId("play-button", "learn-demo-play-btn");
  playButton.append(document.createTextNode("Play"));
  const nextButton = createDivWithClassId("next-button", "learn-demo-next-btn");
  nextButton.append(document.createTextNode("Next"));
  buttonGroup.append(playButton);
  buttonGroup.append(nextButton);

  taskBox.append(displayBox);
  taskBox.append(recordPan);
  taskBox.append(buttonGroup);

  return(taskBox);
}

function createTaskBox (config) {
  const taskBox = createDivWithClassId("task-box", `${config.taskId}`);
  taskBox.append(document.createTextNode(`${config.taskId.slice(4,)}.`));

  const displayBox = createDivWithClassId("display-box", `${config.taskId}-display-box`);
  const taskBg = createDivWithClassId(config.bg, `${config.taskId}-bg`);
  const taskAgent = createDivWithClassId(config.agent, `${config.taskId}-agent`);
  const taskRecipient = createDivWithClassId(config.recipient, `${config.taskId}-recipient`);
  taskBg.append(taskAgent);
  taskBg.append(taskRecipient);
  displayBox.append(taskBg);

  const recordPan = createDivWithClassId("record-pan", `${config.taskId}-record-pan`);

  const buttonGroup = createDivWithClassId("button-group", `${config.taskId}-record-pan`);
  const checkButton = createDivWithClassId("check-button", `${config.taskId}-check-btn`);
  checkButton.append(document.createTextNode("Check"));
  const nextButton = createDivWithClassId("next-button", `${config.taskId}-next-btn`);
  nextButton.append(document.createTextNode("Next"));
  buttonGroup.append(checkButton);
  buttonGroup.append(nextButton);

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

function addText (id, text) {
  let el = document.getElementById(id);
  const t = document.createTextNode(text);
  el.appendChild(t);
}

document.body.append(createLearnDemo(learnDemo));
document.body.append(createTaskBox(task01));

document.getElementById("learn-demo-play-btn").onclick = () => {
  playEffects(learnDemo);
  // setTimeout(() => {
  //     resetBtn.disabled = false;
  //     nextBtn.disabled = false
  // }, 2000);
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
    recipentStone.className = `${config.result}`;
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
