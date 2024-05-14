# Hackathon DSN

Dépôt pour le Hackathon DSN organisé par la Direction interministérielle du numérique.

**📙 Retrouvez le Guide du participant sur Outline en cliquant [ici](https://documentation.beta.numerique.gouv.fr/doc/guide-hackathon-dsn-Vvxa7bq3O0)**

## 👩‍💻 Comment participer ? 

Pour participer au Hackathon vous devez tout d'abord créer un fork de ce dépôt. Pour cela cliquez [ici](https://github.com/etalab-ia/Hackathon-DSN/fork) puis sur *"Create fork"*. Vérifiez que votre fork est bien public dans les settings. 

Vous pouvez déposer votre code sur ce fork, il servira pour l'évaluation de votre projet à l'issu du Hackathon ! 

Bon courage 🔥!

## 🌸 Rendu

Pour évaluer votre projet merci de compléter ce README avec les informations suivantes : 

### Description

* *Problématique et proposition de valeur.*
* *A quelle problématique s’attaque votre projet ?*
* *Quelle est votre proposition de valeur ?*

### Solution

* *Description de la solution et de ses fonctionnalités*
* *Quel usage est fait des données ? Que vous permettent-elles de faire ?*
* *Quelle est la méthode de création de la solution ?*

### Impact envisagé

* *Que permet de faire la solution ?*
* *Qui sont les usagers visés, et qu’en feraient-ils ?*

### Ressources

* *Lien vers la documentation du projet*

  Code SQL pour calculer les CDD et CDI :
  select cdi, cdd, handicap, count(*) from (select max(ind_cdi) as cdi, max(ind_cdd) as cdd, max(ind_handicap) as handicap, id_assure from (
			select id_assure,
			case when nature_contrat in ('01', ' 08', '09') then 1 else 0 end as ind_cdi,
			case when nature_contrat in ('02', '03', '10') then 1 else 0 end as ind_cdd,
			case when d.statut_boeth is not null then 1 else 0 end as ind_handicap
			from ddadtemployeur_assure as c 
			inner join ddadtcontrat as d 
			on c.id = d.id_employeur_assure
			) 
		group by id_assure) group by cdi, cdd, handicap ;

  

### [Facultatif] Retours sur la qualité des données exploitées

* *Quelles sont les difficultés que vous avez rencontrées dans l’usage des données ?*
