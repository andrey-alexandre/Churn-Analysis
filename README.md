---
title: "Procedimentos para experimentação de Churn"
author: "Andrey Alexandre"
output:
html_document: default
word_document: default
---

# Descrição do projeto

Um projeto de experimentação de Churn consiste de 4 etapas.

1. Análise Exploratória dos dados
2. Modelagem
	 + Preditiva
	 + Inferencial
3. Criação de relatório inicial
4. Experimentação
	 + Definição de KPI's
	 + Levantamento de possíveis ações a serem tomadas
	 + Amostragem
	 + Tomada de ação conforme definido por amostragem
	 + Análise longitudinal/causal
5. Automatização parcial da tomada de ação

# Análise Exploratória dos dados

Deve-se criar visualizações e testes estatísticos para averiguar o impacto das variáveis disponíveis no comportamento dos casos de churn. Será de grande proveito manter algumas das visualizações criadas nessa etapa para apresentação posterior.

# Modelagem
## Preditiva

Deve-se criar um modelo capaz de predizer com eficácia os casos de churn. Modelos como XGBoost, Regressão Logística, Árvore de Decisão.

## Inferencial

Deve-se criar um modelo capaz de descrever com eficácia os casos de churn. Modelos como Regressão Logística, Regressão Probíto, entre outras.

# Criação de relatório inicial

Deve-se criar o relatório com as informações achadas na AED e na modelagem inferencial, de forma que o cliente seja capaz de compreender os comportamentos encontrados nos dados e tomar decisões baseadas em dados. É importante que nesta fase sejam utilizadas tanto técnicas visuais quanto técnicas estatísticas para verificação e comprovação das relações encontradas.

O relatório deverá ser composto de duas partes, uma técnica, se aprofundando nas métricas e metodologias utilizadas com intuito de se tornar parte da documentação interna e, caso o cliente queira, parte do entregável. A segunda parte do relatório deverá ser um relatório gerencial contendo os achados das etapas de Modelagem e EAD, tanto de maneira visual quanto escrita.

# Experimentação
## Definição de KPI's

Internamente deve-se utilizar de técnicas estatísticas, como PCA, para gerar possíveis KPI's a serem utilizadas no acompanhamento dos futuros casos de churn.

Externamente deve-se fazer uma ou mais reuniões com o cliente, após a entrega do relatório inicial, para conversar de forma mais específica sobre os achados nas etapas anteriores e com isso definir as KPI's a serem criadas/utilizadas para acompanhar o comportamento dos possíveis casos de churn futuros.

## Levantamento de possíveis ações a serem tomadas

Em conjunto com a definição de KPI's devem ser levantadas ações a serem tomadas pelo cliente para evitar os casos de churn. Esta atividade deverá ser executada, preferencialmente, durante a reunião para que sejam analisadas as tomadas de ação e seus impactos no experimento a ser feito.

## Amostragem

Deve-se verificar se há, na academia casos de análise de churn para a área do cliente, para que se analise possíveis grupos que devam ser separados no momento da amostragem. Esta atividade deve também ser executada junto ao cliente, para verificar se há a percepção de comportamentos distintos entre grupos(Exemplo: Sexo, Raça ou Faixa Etária).

Posteriormente deverá ser feito o cálculo amostral para definir quantos grupos serão criados e quais tratamentos eles receberão. Esta amostragem deverá ser aprovada pelo cliente, principalmente havendo custos.

Na amostragem deverão ser definidos o tamanho amostral, duração da experimentação e, caso haja necessidade, pontos de avaliação durante a experimentação para acompanhamento longitudinal.

## Tomada de ação conforme definido pela amostragem

Após aprovação do plano amostral por parte do cliente, deverá ser iniciado o experimento. É importante que o cliente não receba uma lista discriminando quais são os casos de possível churn para evitar um viés de tratamento por parte do mesmo. Outras restrições e necessidades deverão ser analisadas no momento de início do experimento.
 
## Análise preditiva

Deve-se aplicar técnicas estatísticas para fazer a análise da efetividade dos tratamentos nos casos de churn, assim como uma nova análise preditiva com base nos dados obtidos do experimento.


# Automatização parcial/completa da tomada de ação

Após a análise da efetividade dos tratamentos, deve-se criar métodos de automatização, mesmo que parcial, da tomada de ação como por exemplo o envio de e-mail para o cliente com as informações do possível caso de churn e o tratamento ótimo para o mesmo.