# -*- coding: utf-8 -*-
"""
Clasificación NOVA para alimentos ENNyS Argentina
Basado en Monteiro et al. (2019) y paper Brasil UPF
NOVA 1: Sin procesar o mínimamente procesados
NOVA 2: Ingredientes culinarios procesados
NOVA 3: Procesados
NOVA 4: Ultraprocesados (UPF)

Versión corregida: ajuste de sobre-clasificación en NOVA 4
Ref: distribución objetivo ~33% NOVA1, 6% NOVA2, 19% NOVA3, 43% NOVA4
"""

import pandas as pd
import re
import sys

sys.stdout.reconfigure(line_buffering=True)

# Cargar alimentos
try:
    df = pd.read_csv('alimentos_clasificados.csv', encoding='utf-8')
except UnicodeDecodeError:
    df = pd.read_csv('alimentos_clasificados.csv', encoding='latin-1')


def clasificar_nova(row):
    desc = str(row['descripcion']).lower()
    cod = str(row['codigo'])

    # ==================================================================
    # BLOQUE 0: Excepciones tempranas (alta prioridad)
    # ==================================================================

    # --- NOVA 1: Granos, semillas, tubérculos básicos ---
    # Estos deben capturarse ANTES de los keywords UPF que podrían matchearlos
    if cod.startswith('A'):
        # Granos y semillas enteras
        if any(x in desc for x in [
            'amaranto', 'mijo', 'sorgo', 'centeno, grano', 'cebada, grano',
            'trigo, grano', 'maíz, grano', 'quinoa, semilla',
        ]):
            if not any(x in desc for x in ['inflad', 'azucarad', 'saborizad']):
                return 1
        # Avena arrollada / salvado (mínimamente procesados)
        if 'avena, arrollada' in desc or 'avena arrollada' in desc:
            return 1
        if 'salvado de' in desc and 'bastoncitos' not in desc:
            return 1
        # Legumbres secas
        if any(x in desc for x in [
            'arveja, semilla seca', 'poroto', 'lenteja', 'garbanzo',
            'soja, poroto', 'haba seca'
        ]) and 'enlatad' not in desc and 'texturizada' not in desc:
            return 1
        # Tubérculos y choclo
        if any(x in desc for x in ['choclo', 'papa,', 'batata,', 'mandioca,']):
            if not any(x in desc for x in ['enlatad', 'instantáneo', 'prefrit', 'congelad']):
                return 1
        # Arroz básico
        if any(x in desc for x in ['arroz blanco', 'arroz integral', 'arroz yamaní', 'arroz parboil']):
            if not any(x in desc for x in ['inflad', 'azucarad', 'envasado.*fortificado']):
                return 1
        # Polenta / sémola (mínimamente procesados)
        if 'polenta' in desc and 'instantánea' not in desc and 'premezcla' not in desc:
            return 1

    # --- NOVA 1: Leche en polvo sin saborizar (mínimamente procesada) ---
    if cod.startswith('L'):
        # Leche en polvo básica (entera, descremada, fortificada con vitaminas básicas)
        if 'en polvo' in desc:
            # Excluir las que SÍ son UPF
            if any(x in desc for x in [
                'chocolatada', 'sabor', 'café', 'fórmula', 'nido buen día',
                'fortigrow', 'ensure', 'pediasure', 'nesquik'
            ]):
                return 4
            # Leche en polvo simple = NOVA 1
            return 1
        # Leche fluida
        if any(x in desc for x in ['fluida', 'ordeñada']):
            if any(x in desc for x in ['chocolatada', 'saborizad', 'fórmula']):
                return 4
            return 1
        # Leche entera/descremada líquida (UHT, pasteurizada)
        if any(x in desc for x in ['entera', 'descremada', 'semidescremada']):
            if any(x in desc for x in ['chocolatada', 'saborizad', 'fórmula', 'café']):
                return 4
            if 'condensada' in desc:
                return 3
            return 1

    # --- NOVA 1: Tofu (mínimamente procesado de soja) ---
    if 'tofu' in desc and 'saborizad' not in desc:
        return 1

    # --- NOVA 1: Yogur natural sin aditivos ---
    if cod in ['Y001']:
        return 1

    # --- NOVA 3: Bebidas alcohólicas artesanales/tradicionales ---
    if cod.startswith('B'):
        if any(x in desc for x in [
            'vino blanco', 'vino tinto', 'vino rosado', 'vino espumante',
            'vino mistela', 'champagne', 'cerveza con alcohol', 'cerveza artesanal',
            'sidra', 'fernet', 'whisky', 'vodka', 'gin', 'ron', 'licor',
            'aperitivo', 'grapa', 'caña', 'coñac', 'cognac', 'brandy',
            'aguardiente', 'pisco'
        ]):
            # Mezclas listas = UPF
            if any(x in desc for x in ['lista para consumir', 'lata', 'premix', 'con gaseosa']):
                return 4
            return 3

    # --- NOVA 3: Dulces tradicionales artesanales ---
    if 'pickles' in desc or 'en vinagre' in desc:
        return 3
    if 'aceituna' in desc:
        return 3
    if 'tomate' in desc and 'puré' in desc and 'instantáneo' not in desc:
        return 3

    # --- NOVA 3: Leche condensada, dulce de leche, mermelada artesanal ---
    # Nota: dulce de leche y mermelada son debatidos; los clasificamos como NOVA 3
    # (procesados) cuando no son industriales con aditivos
    if 'leche condensada' in desc:
        return 3

    # ==================================================================
    # BLOQUE 1: Patrones para NOVA 4 (Ultraprocesados)
    # ==================================================================
    upf_keywords = [
        # Términos generales de procesamiento industrial
        'industrializad', 'premezcla', 'instantáne',
        'light', 'dietétic', 'saborizad',
        'edulcorant',
        # Bebidas UPF
        'gaseosa', 'bebida energizante', 'jugo en polvo', 'jugo concentrado',
        'agua saborizada', 'bebida a base de soja', 'gatorade', 'powerade',
        'té concentrado', 'anana fizz', 'dr. lemon', 'granadina',
        # Cereales de desayuno y barras
        'cereal desayuno', 'cereal infantil', 'barra de cereal',
        'cereal.*aritos', 'cereal.*bolitas', 'cereal.*copos azucarados',
        'barra crocante', 'barra proteica',
        'trigo inflado azucarado', 'arroz inflado azucarado', 'quinoa inflada',
        'avena.*almohaditas', 'tutuca', 'pochoclo acaramelado',
        # Galletitas y panificados industriales
        'galletitas', 'galleta', 'alfajor', 'bizcochuelo',
        'budín industrializado', 'madalenas', 'muffins',
        'pan lactal', 'pan de molde', 'pan para panchos',
        'tortillas.*envasadas', 'tostaditas.*saborizadas',
        # Snacks
        'snack', 'copetín', 'palitos', 'papas fritas de copetín',
        'nachos', 'maní japonés',
        # Golosinas y chocolates
        'chocolate', 'golosina', 'caramelo', 'chicle',
        'chocolatín', 'rhodesia', 'ducrem', 'mantecol',
        'oblea.*kit kat', 'minitorta bañada', 'bombón.*bon o bon',
        'chocolate.*snickers', 'huevo.*kinder', 'turrón.*golosina',
        # Lácteos UPF
        'leche chocolatada', 'postre de leche', 'flan envasado',
        'yogur.*cereales', 'yogur.*frutas', 'bebida láctea',
        'leche fórmula', 'leche de almendras', 'leche de castañas',
        'leche de coco.*industrializada',
        # Carnes procesadas industriales
        'hamburguesa.*industrializad', 'nuggets',
        'milanesa.*prefrita', 'suprema.*prefrita',
        'salchicha', 'mortadela', 'jamón cocido', 'fiambre',
        # Salsas y condimentos industriales
        'sopa.*polvo', 'sopa.*instantánea', 'caldo en cubo', 'caldo en polvo',
        'salsa.*polvo', 'aderezo', 'ketchup', 'mayonesa',
        'polvo para preparar', 'gelatina.*polvo',
        # Comidas preparadas industriales
        'puré instantáneo', 'medallones.*congelados', 'prefritos congelados',
        'arroz.*envasado.*fortificado', 'guiso.*envasado',
        'pizza.*premezcla', 'masa.*premezcla', 'ñoquis.*premezcla',
        'ravioles.*envasados', 'capelettis.*deshidratados',
        'fideos.*sin tacc', 'pan rallado.*fortificado',
        # Otros UPF
        'café.*cápsula', 'capuccino instantáneo', 'malta instantánea',
        'mcdonald', 'comida rápida',
        'crema chantilly', 'margarina', 'rocío vegetal',
        'mantequilla de maní', 'mix.*frutos secos.*envasado',
        'soja texturizada', 'hamburguesa de quinoa', 'hamburguesa de soja',
        'seitán', 'bocaditos de pescado', 'kani kama', 'anchoa.*pasta',
        'caldo de verduras colado', 'zapallo.*puré instantáneo',
        'frutas abrillantadas', 'anana glaseado', 'coctail.*enlatado',
        'puré de frutas.*infantil',
        'dulce de leche', 'mermelada', 'jalea',
        'salsa topping', 'relleno.*repostería',
        'baño de repostería', 'fondant', 'polvo.*mousse',
        'cacao.*nesquik', 'cacao.*zucoa',
        'helado.*envasado', 'torta helada',
        'azúcar light', 'glucosa', 'dextrosa',
        'saborizador', 'saborizadores',
        'suplemento', 'herbalife', 'ensure', 'fresubin', 'glucerna', 'pediasure',
        'cous cous', 'harina.*levadura', 'harina leudante',
        # Específicos: "envasad" solo en contextos claramente UPF
        'envasad.*listo para consumir', 'envasad.*fortificad',
    ]

    for kw in upf_keywords:
        if re.search(kw, desc):
            return 4

    # ==================================================================
    # BLOQUE 2: NOVA 2 (Ingredientes culinarios procesados)
    # ==================================================================
    nov2_keywords = [
        'aceite', 'azúcar', 'sal', 'vinagre', 'miel', 'manteca', 'grasa',
        'almidón', 'tapioca', 'fécula', 'bicarbonato', 'levadura', 'polvo de hornear',
        'cacao amargo', 'cascarita', 'gomasio', 'sal marina', 'sal dietética',
        'cloruro de sodio',
    ]
    for kw in nov2_keywords:
        if re.search(kw, desc):
            if 'aceite' in kw and ('pescado' in desc or ('chía' in desc and 'semilla' in desc)):
                continue
            return 2

    # NOVA 2 específicos
    if cod.startswith('Z'):
        if any(x in desc for x in ['leche', 'almendras', 'coco', 'castañas', 'chocolate', 'bebida']):
            return 4
        if 'aceite' in desc or 'semilla' in desc or 'almendra' in desc or 'avellana' in desc:
            if 'mantequilla de maní' in desc or 'mix de frutos' in desc:
                return 4
            return 2 if 'aceite' in desc else 1
    if cod in ['D003', 'D004', 'D011', 'D026', 'D027', 'D105', 'D116', 'D134'] and 'light' not in desc:
        return 2
    if cod.startswith('G'):
        if 'margarina' in desc or 'rocío' in desc or 'chantilly' in desc:
            return 4
        return 2

    # Harinas simples (sin levadura/leudante/premezcla) = NOVA 2
    if cod.startswith('A'):
        if any(x in desc for x in ['harina de', 'sémola', 'almidón de', 'fécula de']):
            if not any(x in desc for x in ['levadura', 'leudante', 'premezcla']):
                return 2

    # ==================================================================
    # BLOQUE 3: NOVA 3 (Procesados)
    # ==================================================================
    nov3_keywords = [
        'en conserva', 'en escabeche', 'encurtido', 'orejón', 'ciruela pasa',
        'higo desecado', 'dátiles', 'coco rallado', 'ricota',
        'jamón crudo', 'queso', 'chorizo seco',
        'pan francés', 'pan criollo', 'pan árabe', 'pan cacho', 'pan negro',
        'pan de centeno', 'pan de maíz', 'pan de miga', 'pan de viena',
        'factura', 'bizcocho.*panadería', 'criollito', 'chipá', 'tortilla',
        'fideos secos', 'fideos frescos', 'pasta seca',
        'atún.*enlatado', 'caballa.*enlatada', 'sardina.*enlatada', 'anchoa.*enlatada',
        'dulce de batata', 'dulce de membrillo',
        'arrope', 'almíbar', 'charqui', 'pan rallado',
        'cerveza sin alcohol',
    ]
    for kw in nov3_keywords:
        if re.search(kw, desc):
            if 'pan para panchos' in desc or 'pan lactal' in desc or 'pan de molde' in desc:
                return 4
            return 3

    # Más NOVA 3
    if 'enlatad' in desc and not any(x in desc for x in ['gaseosa', 'jugo', 'bebida']):
        return 3
    if 'desecad' in desc or 'pasas' in desc:
        return 3

    # ==================================================================
    # BLOQUE 4: Reglas por categoría de alimento
    # ==================================================================

    # Carne fresca
    if cod.startswith('C'):
        if not any(x in desc for x in [
            'industrializad', 'prefrit', 'nuggets', 'hamburguesa', 'fiambre',
            'bondiola', 'jamón', 'salame', 'chorizo', 'mortadela', 'salchicha',
            'longaniza', 'pastrón', 'cantimpalo'
        ]):
            return 1
        # Embutidos y curados artesanales = NOVA 3
        if any(x in desc for x in ['bondiola', 'jamón crudo', 'salame', 'chorizo seco',
                                     'longaniza', 'pastrón', 'cantimpalo']):
            return 3
        return 4

    # Pescado fresco
    if cod.startswith('P'):
        if not any(x in desc for x in ['enlatad', 'prefrit', 'bocaditos', 'kani kama']):
            return 1
        if 'enlatad' in desc:
            return 3
        return 4

    # Huevo
    if cod.startswith('H'):
        return 1

    # Leche (los que no fueron capturados antes)
    if cod.startswith('L'):
        if any(x in desc for x in ['chocolatada', 'saborizad', 'fórmula', 'café',
                                     'postre', 'flan', 'fortified with iron']):
            return 4
        if 'condensada' in desc:
            return 3
        return 1  # default leche = mínimamente procesada

    # Verduras
    if cod.startswith('V'):
        if any(x in desc for x in ['industrializados', 'prefritos', 'instantáneo']):
            return 4
        if any(x in desc for x in ['congelada', 'enlatad', 'en conserva', 'en escabeche', 'encurtido']):
            return 3
        return 1

    # Frutas
    if cod.startswith('F'):
        if any(x in desc for x in ['enlatad', 'glaseado', 'abrillantadas', 'industrializada']):
            return 4
        if 'desecad' in desc or 'orejón' in desc or 'pasa' in desc:
            return 3
        return 1

    # Cereales y legumbres (A) - los que no fueron capturados antes
    if cod.startswith('A'):
        # Fideos y pastas simples
        if any(x in desc for x in ['fideos secos', 'fideos frescos', 'pasta seca']):
            return 3
        # Ñoquis de papa frescos/caseros (no envasados industrial)
        if 'ñoquis' in desc and 'premezcla' not in desc and 'envasad' not in desc:
            return 3
        # Pan artesanal
        if any(x in desc for x in ['pan francés', 'pan criollo', 'pan árabe', 'pan casero']):
            return 3
        # Facturas de panadería
        if 'factura' in desc or 'medialuna' in desc or 'criollito' in desc:
            return 3
        # Cualquier otra A que quede: probablemente es procesado o UPF industrial
        # Verificar si tiene indicadores de UPF
        if any(x in desc for x in ['envasad', 'fortificad', 'inflad', 'azucarad',
                                     'relleno', 'cobertura', 'bañado']):
            return 4
        # Default A: procesado (NOVA 3) en lugar de UPF
        return 3

    # Quesos
    if cod.startswith('Q'):
        # Queso untable/procesado con sabor = UPF
        if any(x in desc for x in ['saborizad', 'light', 'untable.*sabor']):
            return 4
        return 3

    # Yogures (excepto Y001 ya capturado como NOVA 1)
    if cod.startswith('Y'):
        # Yogur natural batido/firme sin frutas ni cereales = NOVA 1
        if any(x in desc for x in ['natural', 'batido natural', 'firme natural']):
            return 1
        # Yogur con frutas, cereales, sabores = UPF
        return 4

    # Bebidas
    if cod.startswith('B'):
        # Agua, té, café, mate, infusiones sin saborizar = NOVA 1
        if any(x in desc for x in ['agua', 'té', 'café', 'mate', 'infusión']):
            if any(x in desc for x in ['saborizada', 'aquarius', 'gatorade', 'gaseosa']):
                return 4
            return 1
        # Jugo de fruta natural/exprimido = NOVA 1
        if 'jugo' in desc and 'natural' in desc:
            return 1
        # Resto de bebidas = UPF (jugos industriales, gaseosas, etc.)
        return 4

    # Aceites, frutas secas y semillas (Z)
    if cod.startswith('Z'):
        if any(x in desc for x in ['nuez', 'almendra', 'avellana', 'castaña', 'pistacho',
                                     'maní', 'semilla de', 'girasol', 'sésamo', 'chía',
                                     'lino', 'zapallo semilla']):
            if 'mantequilla de maní' in desc or 'mix de frutos' in desc:
                return 4
            return 1
        if 'aceite' in desc:
            return 2
        return 1  # frutos secos y semillas = NOVA 1

    # Suplementos
    if cod.startswith('S'):
        return 4

    # Comidas preparadas (recetas)
    if cod.startswith('R'):
        if any(x in desc for x in ['mcdonald', 'comida rápida', 'pizza congelada']):
            return 4
        # Comidas preparadas caseras/tradicionales = NOVA 3
        return 3

    # Dulces (D) no capturados antes
    if cod.startswith('D'):
        return 4

    # Opcionales (O)
    if cod.startswith('O'):
        if any(x in desc for x in ['pimienta', 'orégano', 'comino', 'pimentón',
                                     'nuez moscada', 'canela', 'clavo', 'laurel',
                                     'perejil', 'ají molido', 'curry', 'cúrcuma',
                                     'provenzal', 'chimichurri casero']):
            return 2  # Especias y condimentos simples
        return 4  # Aderezos industriales, caldos, etc.

    # Default global: NOVA 3
    return 3


# Aplicar clasificación
df['NOVA'] = df.apply(clasificar_nova, axis=1)

# Guardar
df.to_csv('alimentos_clasificados_NOVA.csv', index=False, encoding='utf-8-sig')
print(f"Clasificados {len(df)} alimentos")
print(df['NOVA'].value_counts().sort_index())

# Comparar con referencia
print("\n=== COMPARACIÓN CON REFERENCIA ===")
ref = {1: 343, 2: 60, 3: 193, 4: 444}
ref_total = 1040
df_no_s = df[~df['codigo'].str.startswith('S')]
counts = df_no_s['NOVA'].value_counts().sort_index()
total = len(df_no_s)
print(f"{'NOVA':<8} {'Nuestro':<10} {'%':<8} {'Ref':<8} {'Ref%':<8} {'Dif':<8}")
print("-" * 50)
for k in [1, 2, 3, 4]:
    n = counts.get(k, 0)
    p = n / total * 100
    rn = ref[k]
    rp = rn / ref_total * 100
    print(f"NOVA {k:<3} {n:<10} {p:<8.1f} {rn:<8} {rp:<8.1f} {p - rp:+.1f}")
