const SERVER_ADDRESS = "localhost:63219";

window.addEventListener('DOMContentLoaded', function () {
    document.getElementById("play-button").onclick = async function () {
        var input = document.getElementById("code").textContent;

        var result = await fetch(SERVER_ADDRESS + "/" + "compile", {
            body: input
        });

        document.getElementById("term").textContent = result.body;
    }
});

