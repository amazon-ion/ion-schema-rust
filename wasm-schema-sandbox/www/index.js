import * as ion from "wasm-ion-schema";

const validateButton = document.getElementById("validate");
function loadPage() {
    document.getElementById("schema_type").setAttribute("placeholder", "e.g. my_type");
    document.getElementById("schema_type").value = "";
    var schemaEditor = ace.edit("schema");
    schemaEditor.setOptions({
        mode: 'ace/mode/ion',
        theme: 'ace/theme/base',
        placeholder: "Write your schema here!\ne.g. type::{ name: my_type, type: string }",
    });

    var valueEditor = ace.edit("value");
    valueEditor.setOptions({
        mode: 'ace/mode/ion',
        theme: 'ace/theme/base',
        placeholder: "e.g. \"hello\"",
    });
}

loadPage();
const show = () => {
    const pre = document.getElementById('result');
    const schemaContent = document.getElementById('schema').getElementsByClassName("ace_scroller").item(0).innerText;
    const valueContent = document.getElementById('value').getElementsByClassName("ace_scroller").item(0).innerText;

    const result = ion.validate(valueContent, schemaContent, document.getElementById('schema_type').value);
    const violation = document.getElementById('violation');
    violation.textContent = result.violation();

    if (result.has_error()) {
        alert(result.error());
    }
    if (result.result()) {
        pre.style.color = "#009926";
        pre.textContent = result.value() + " is valid!";
    } else {
        if (result.has_error()) {
            pre.textContent = "";
        } else {
            pre.style.color = "#ff0000";
            pre.textContent = result.value() + " is invalid!\n";
        }
    }
    console.log(result);
};

validateButton.addEventListener("click", event => {
    show()
});
