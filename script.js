const ESTRUTURA = {
    pinca: ["MD_PT1", "MD_PT2", "MD_PF", "MD_P"],
    manip: ["MT_S", "MT_C", "MT_F", "MR_Q", "MR_C"],
    grosso: ["CB_A", "CB_R", "CP_P", "CE_P1", "CE_P2", "CE_T"]
};

const LABELS_ITENS = {
    MD_PT1: "Pinos na tábua - Mão preferida",
    MD_PT2: "Pinos na tábua - Mão não preferida",
    MD_PF: "Mudar pinos",
    MD_P: "Pesponto",
    MT_S: "Traçado simples",
    MT_C: "Traçado complexo",
    MT_F: "Figuras geométricas",
    MR_Q: "Recorte reto",
    MR_C: "Recorte curvo",
    CB_A: "Agarrar bola com duas mãos",
    CB_R: "Agarrar bola com mão preferida",
    CP_P: "Polichinelo",
    CE_P1: "Equilíbrio - perna preferida",
    CE_P2: "Equilíbrio - perna não preferida",
    CE_T: "Tandem"
};

const DIAG_TEXTOS = {
    Diag1: {
        muitoabaixo: "Dificuldade severa na precisão digital e preensão de pequenos objetos. ",
        abaixo: "Habilidade de pinça fina abaixo do esperado para a idade cronológica. ",
        limitrofe: "Desempenho de pinça em limiar de atenção; oscilações na precisão. ",
        normal: "Coordenação motora fina (pinça) adequada e funcional. ",
        limitrofe_sup: "Boa precisão digital, com desempenho levemente acima da média. ",
        acima: "Excelente controle motor fino e destreza digital refinada. ",
        muitoacima: "Coordenação de pinça excepcional, demonstrando maturidade motora precoce. "
    },
    Diag2: {
        muitoabaixo: "Falhas críticas no manejo de ferramentas e coordenação visomotora. ",
        abaixo: "Dificuldade moderada em tarefas que exigem manipulação e corte. ",
        limitrofe: "A manipulação de objetos apresenta lentidão ou imprecisão leve. ",
        normal: "Habilidades de manipulação manual seguem o padrão normativo. ",
        limitrofe_sup: "Capacidade de manipulação segura e eficiente de materiais diversos. ",
        acima: "Alta competência em tarefas manuais complexas e sequenciais. ",
        muitoacima: "Domínio técnico superior no uso de ferramentas e objetos manuais. "
    },
    Diag3: {
        muitoabaixo: "Comprometimento significativo no equilíbrio e coordenação global. ",
        abaixo: "Desempenho motor grosso instável, com dificuldades em posturas estáticas. ",
        limitrofe: "Coordenação global dentro do limite inferior; equilíbrio sensível. ",
        normal: "Padrões de locomoção e equilíbrio compatíveis com a faixa etária. ",
        limitrofe_sup: "Bom controle postural e segurança em atividades de movimento. ",
        acima: "Ótima consciência corporal e equilíbrio dinâmico desenvolvido. ",
        muitoacima: "Performance motora grossa excelente, com agilidade e estabilidade ímpares. "
    },
    Diag4: {
        muitoabaixo: "O perfil geral indica atraso global no desenvolvimento psicomotor.",
        abaixo: "Resultado total sugere necessidade de estimulação motora focada.",
        limitrofe: "Perfil geral limítrofe, recomendando-se acompanhamento preventivo.",
        normal: "O desenvolvimento motor global encontra-se dentro da normalidade.",
        limitrofe_sup: "Desempenho geral sólido, com boas competências psicomotoras.",
        acima: "Perfil motor global altamente desenvolvido e harmonioso.",
        muitoacima: "Excelência em todas as dimensões motoras avaliadas pelo teste."
    }
};

const ITENS = [...ESTRUTURA.pinca, ...ESTRUTURA.manip, ...ESTRUTURA.grosso];
const ITENS_INVERTIDOS = ["MD_PT1","MD_PT2","MD_PF","MD_P","MT_S","MT_C","MR_Q","MR_C"];

function formatarT(valor) {
    if (valor <= 20) return "≤20";
    if (valor >= 80) return "≥80";
    return Math.round(valor).toString();
}

function obterInterpretacao(v) {
    if (v < 30) return {cl:"Muito Abaixo", c:"#D32F2F", key:"muitoabaixo"};
    if (v < 35) return {cl:"Abaixo", c:"#F57C00", key:"abaixo"};
    if (v < 46) return {cl:"Limítrofe", c:"#FBC02D", key:"limitrofe"};
    if (v < 55) return {cl:"Normal", c:"#689F38", key:"normal"};
    if (v < 61) return {cl:"Limítrofe (Sup.)", c:"#FBC02D", key:"limitrofe_sup"};
    if (v < 71) return {cl:"Acima", c:"#1976D2", key:"acima"};
    return {cl:"Muito Acima", c:"#7B1FA2", key:"muitoacima"};
}

function calcularIdade() {
    const nasc = document.getElementById("dataNasc").value;
    const exame = document.getElementById("dataExame").value;
    if (!nasc || !exame) return null;
    const idade = (new Date(exame + "T00:00:00") - new Date(nasc + "T00:00:00")) / (1000 * 60 * 60 * 24 * 365.25);
    document.getElementById("exibirIdade").innerText = idade > 0 ? `Idade: ${idade.toFixed(2)} anos` : "";
    return idade;
}

function escoreT(valor, idade, fator) {
    const iB = Math.floor(idade), iP = Math.min(iB + 1, 8), p = idade - iB;
    const m = json.normas_idade[iB][fator].media + p * (json.normas_idade[iP][fator].media - json.normas_idade[iB][fator].media);
    const sd = json.normas_idade[iB][fator].dp + p * (json.normas_idade[iP][fator].dp - json.normas_idade[iB][fator].dp);
    return ((valor - m) / sd) * 10 + 50;
}

/* Definição dos limites para validação */
const LIMITES = {
    // Grupo 1: > 0
    MD_PT1: { min: 0.0001, msg: "maior que 0" },
    MD_PT2: { min: 0.0001, msg: "maior que 0" },
    MD_PF:  { min: 0.0001, msg: "maior que 0" },
    MD_P:   { min: 0.0001, msg: "maior que 0" },
    // Grupo 2: >= 0 (padrão, mas explícito aqui)
    MT_S:   { min: 0, msg: "maior ou igual a 0" },
    MT_C:   { min: 0, msg: "maior ou igual a 0" },
    MT_F:   { min: 0, msg: "maior ou igual a 0" },
    MR_Q:   { min: 0, msg: "maior ou igual a 0" },
    MR_C:   { min: 0, msg: "maior ou igual a 0" },
    // Grupo 3: Restrições específicas
    CB_A:   { min: 0, max: 5, msg: "entre 0 e 5" },
    CB_R:   { min: 0, max: 5, msg: "entre 0 e 5" },
    CP_P:   { min: 1, max: 3, msg: "entre 1 e 3" },
    CE_P1:  { min: 0, msg: "maior ou igual a 0" },
    CE_P2:  { min: 0, msg: "maior ou igual a 0" },
    CE_T:   { min: 0, msg: "maior ou igual a 0" }
};

function validarECalcular() {
    const idade = calcularIdade();
    if (!idade || idade < 4 || idade >= 9) {
        alert("Idade fora da faixa normativa (4 a 8 anos).");
        return;
    }

    let respostas = {};
    
    // Validação de limites
    for (let item of ITENS) {
        const input = document.getElementById(item);
        const valRaw = input.value;
        
        // Se estiver em branco, aceita e segue para lógica de imputação
        if (valRaw === "") {
            const piorDesempenho = ITENS_INVERTIDOS.includes(item) ? json.maximos_itens[item] : json.minimos_itens[item];
            respostas[item] = ITENS_INVERTIDOS.includes(item) ? (json.maximos_itens[item] + json.minimos_itens[item]) - piorDesempenho : piorDesempenho;
            continue;
        }

        const val = parseFloat(valRaw);
        const regra = LIMITES[item];

        // Checagem de limites
        if (regra) {
            if ((regra.min !== undefined && val < regra.min) || (regra.max !== undefined && val > regra.max)) {
                alert(`O item "${LABELS_ITENS[item]}" deve ter valor ${regra.msg}.`);
                input.focus();
                return; // Interrompe o cálculo
            }
        }

        // Lógica de inversão para valores preenchidos
        respostas[item] = ITENS_INVERTIDOS.includes(item) ? (json.maximos_itens[item] + json.minimos_itens[item]) - val : val;
    }

    // Se passou na validação, calcula latentes
    const lat = {F1:0, F2:0, F3:0};
    ITENS.forEach(item => {
        const xc = respostas[item] - json.medias_itens[item];
        lat.F1 += xc * json.pesos_1ordem.F1[item];
        lat.F2 += xc * json.pesos_1ordem.F2[item];
        lat.F3 += xc * json.pesos_1ordem.F3[item];
    });
    lat.G = lat.F1 * json.pesos_geral.F1 + lat.F2 * json.pesos_geral.F2 + lat.F3 * json.pesos_geral.F3;

    renderizar({
        F1: escoreT(lat.F1, idade, "F1"),
        F2: escoreT(lat.F2, idade, "F2"),
        F3: escoreT(lat.F3, idade, "F3"),
        Geral: escoreT(lat.G, idade, "G")
    });
}

window.onload = () => {
    document.getElementById("dataExame").value = new Date().toISOString().split("T")[0];
    const fill = (boxId, keys) => {
        const container = document.getElementById(boxId);
        keys.forEach(k => {
            const regra = LIMITES[k];
            // Adiciona atributos min e max nativos do HTML para auxiliar o usuário
            const minAttr = regra && regra.min !== undefined ? `min="${regra.min}"` : "";
            const maxAttr = regra && regra.max !== undefined ? `max="${regra.max}"` : "";
            
            container.innerHTML += `
                <div class="item-row">
                    <label title="${LABELS_ITENS[k]}">${LABELS_ITENS[k]}</label>
                    <input type="number" id="${k}" step="any" ${minAttr} ${maxAttr}>
                </div>`;
        });
    };
    fill("box-fino-pinca", ESTRUTURA.pinca);
    fill("box-fino-manip", ESTRUTURA.manip);
    fill("box-grosso", ESTRUTURA.grosso);
    
    document.getElementById("dataNasc").addEventListener("change", calcularIdade);
    document.getElementById("dataExame").addEventListener("change", calcularIdade);
};

/* ... (Mantenha as constantes ESTRUTURA, LABELS_ITENS, DIAG_TEXTOS, LIMITES e funções de cálculo) ... */

function renderizar(res) {
    document.getElementById("resultado").style.display = "block";
    const f1 = obterInterpretacao(res.F1);
    const f2 = obterInterpretacao(res.F2);
    const f3 = obterInterpretacao(res.F3);
    const g = obterInterpretacao(res.Geral);

    updateScoreBox("f1-box", "Motor Fino - Pinça", res.F1, f1);
    updateScoreBox("f2-box", "Motor Fino - Manipulação", res.F2, f2);
    updateScoreBox("f3-box", "Motor Grosso", res.F3, f3);
    
    const cardG = document.getElementById("card-geral");
    cardG.style.backgroundColor = g.c;
    cardG.innerHTML = `<small>ESCORE GERAL</small><h1>${formatarT(res.Geral)}</h1><p>${g.cl}</p>`;

    document.getElementById("interpretacao-texto").innerHTML = 
        `<strong>Interpretação Clínica:</strong><br>` +
        DIAG_TEXTOS.Diag1[f1.key] + DIAG_TEXTOS.Diag2[f2.key] + 
        DIAG_TEXTOS.Diag3[f3.key] + DIAG_TEXTOS.Diag4[g.key];

    const haExtremos = [res.F1, res.F2, res.F3, res.Geral].some(v => v <= 20 || v >= 80);
    const nota = document.getElementById("nota-extremos");
    if (haExtremos) {
        nota.innerText = "Valores ≤20 e ≥80 indicam desempenho extremamente distante da média normativa.";
        nota.style.display = "block";
    } else {
        nota.style.display = "none";
    }
}

function updateScoreBox(id, label, val, interp) {
    const el = document.getElementById(id);
    // Mantemos a cor do texto da interpretação, mas o background é controlado pelo CSS (f2 e f3)
    el.innerHTML = `<small>${label}</small><h1>${formatarT(val)}</h1><p style="color:${interp.c}">${interp.cl}</p>`;
}

window.onload = () => {
    document.getElementById("dataExame").value = new Date().toISOString().split("T")[0];
    const fill = (boxId, keys) => {
        const container = document.getElementById(boxId);
        keys.forEach(k => {
            const regra = LIMITES[k];
            const minAttr = regra && regra.min !== undefined ? `min="${regra.min}"` : "";
            const maxAttr = regra && regra.max !== undefined ? `max="${regra.max}"` : "";
            
            container.innerHTML += `
                <div class="item-row">
                    <label title="${LABELS_ITENS[k]}">${LABELS_ITENS[k]}</label>
                    <input type="number" id="${k}" step="any" ${minAttr} ${maxAttr}>
                </div>`;
        });
    };
    fill("box-fino-pinca", ESTRUTURA.pinca);
    fill("box-fino-manip", ESTRUTURA.manip);
    fill("box-grosso", ESTRUTURA.grosso);
    
    document.getElementById("dataNasc").addEventListener("change", calcularIdade);
    document.getElementById("dataExame").addEventListener("change", calcularIdade);
};