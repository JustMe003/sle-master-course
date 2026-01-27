 let x1_value = 0;
 let x3_value = [];
 let x4_value = 0;



// answerable questions

function input_32_2__2_14___2_16_(n) {
  let input;
  input = document.getElementById("input-32,2,<2,14>,<2,16>");
  x1_value = Number(input.value);
  
  repeat_59_2__4_12___4_14_()

}

function input_89_2__5_23___5_25_(n) {
  let input;
  if (n != undefined) {
    input = document.getElementById("input-89,2,<5,23>,<5,25>-" + n);
    x3_value[n] = Number(input.value)
  }
  
  comp_140_2__8_29___8_31_()

}


// computable questions

function comp_140_2__8_29___8_31_() {
  x4_value = Number(x3_value[0] + x3_value[1] + x3_value[2]);
  const computed_element = document.getElementById("comp-140,2,<8,29>,<8,31>");
  computed_element.textContent = x4_value;

}


// If statements


// If else statements


// Repeat statements

function repeat_59_2__4_12___4_14_() {
  const rep = x1_value;
  const template = document.getElementById("repeat-59,2,<4,12>,<4,14>");
  const parent = document.getElementById("repeat-59,2,<4,12>,<4,14>-parent");
  template.style = "display: none;"
  let i = 0;
  while (i < rep) {
    const el = document.getElementById("repeat-59,2,<4,12>,<4,14>-" + i);
    if (el == undefined) {
      const clone = template.cloneNode(true);
      clone.style = "";
      clone.id = clone.id + "-" + i;
      modifyIdOfAllChildren(clone.childNodes, i);
      parent.appendChild(clone);
    } else {
      el.style = "";
    }
    i++;
  }
  let el = document.getElementById("repeat-59,2,<4,12>,<4,14>-" + i);
  while (el != undefined) {
    el.style = "display: none;";
    i++;
    el = document.getElementById("repeat-59,2,<4,12>,<4,14>-" + i);
  }
}


// Calling each function at least once
function init() {
  
  
  input_32_2__2_14___2_16_();

  
  repeat_59_2__4_12___4_14_()
  
  input_89_2__5_23___5_25_();

  
  comp_140_2__8_29___8_31_();

  
}

function getCall(s) {
  return s.replaceAll(/[-<>,]/g, "_");
}

function modifyIdOfAllChildren(childs, i) {
  for (let k = 0; k < childs.length; k++) {
    const c = childs[k];
    if (c.onchange) {
      const s = (' ' + c.id).slice(1);
      c.onchange = function() { eval(getCall(s))(i); };
    }
    if (c.id) c.id = c.id + "-" + i;
    if (c.childNodes) modifyIdOfAllChildren(c.childNodes, i);
  }
}
  