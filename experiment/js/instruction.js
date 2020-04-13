
const descBtn = document.getElementById('desc-btn');
descBtn.onclick = () => {
  document.getElementById("instruction").style.display = "none";
  document.getElementById("comprehension").style.display = "block";
}

const checkBtn = document.getElementById('check-btn');
const checks = [ 'check1', 'check2', 'check3', 'check4' ];
const answers = [ false, true, false, true ];

const passBtn = document.getElementById('pass-btn');
const retryBtn = document.getElementById('retry-btn');


checkBtn.onclick = () => checkComprehension();
passBtn.onclick = () => location.href='task.html';
retryBtn.onclick = () => location.href='instruction.html';

document.getElementById('prequiz').onchange = () => isFilled() ? checkBtn.disabled = false : null;

function checkComprehension() {
    let inputs = [];
    checks.map(check => {
        const vals = document.getElementsByName(check);
        inputs.push(vals[0].checked);
    });
    const pass = (inputs.join('') === answers.join(''));
    showPostCheckPage(pass);
}

function showPostCheckPage (isPass) {
    const pageDiv = isPass? 'pass' : 'retry';
    document.getElementById('check-btn').style.display = 'none';
    document.getElementById(pageDiv).style.display = 'block';
}

function isFilled () {
    let radios = document.getElementsByTagName('input');
    let checked = 0;
    for (let i = 0; i < radios.length; i++) {
        checked += radios[i].checked;
    }
    return (checked > checks.length-1)
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
