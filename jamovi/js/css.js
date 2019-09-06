
const css = `

#sim {
    display: inline-block ;
    margin-left: 12px ;
    padding: 12px ;
    background: #3498db;
    background-image: linear-gradient(to bottom, #3498db, #2980b9);
    box-shadow: 0px 1px 3px #666666;
    color: white
}

#sim:hover {
    background: #3cb0fd;
    background-image: linear-gradient(to bottom, #3cb0fd, #3498db);
}

#sim:active {
    background: #3498db;
    background-image: linear-gradient(to bottom, #3498db, #2980b9);
}

`;
let node = document.createElement('style');
node.innerHTML = css;
document.body.appendChild(node);

module.exports = undefined;
