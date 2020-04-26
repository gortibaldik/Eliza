# Lemmatization and stemming

- its considerably harder to detect keywords in Slovak than in English mainly because of declinations of nouns and conjugation of verbs

The scripts below are examples of transformations applied to input text.

### Sorry script

During the implementation of Eliza I've chosen 2 basic approaches to detection of word 'sorry'. Since there are many ways of saying 'sorry' in Slovak I definitely needed to transform its synonyms into 'sorry'. E.G.  (```I'm sorry = I apologize```) <=> (```prepáč = ospravedlňujem sa```)

- #### First approach :

  - simple stemming :

    - ```prepac = prepac, prepacte = prepac```

  - simple lemmatization :

    - ```ospravedlnujem = prepac, ospravedlnte = prepac, osprave... = prepac``` 

  - ##### Problems :

    - Keyword 'sorry' doesn't have high priority. When the user says ```my sister apologized for her actions``` the keyword isn't ```(apologized = sorry)``` but ```my```. Because of lemmatization Eliza would obtain sentence ```my sister sorry for her actions``` which is an obvious nonsense. 

- #### Second approach :

  - no stemming and no lemmatization in preprocessing

  - lemmatization and stemming only in keyword_matching phase

  - ##### Problems :

    - Eliza obtains sentence in the original form, so she can infer transformation in answering_phase.  There are many problems in conjugation and declination transformations, but not in the 'sorry' script.

### Family script

My implementation of Eliza is hugely inspired by works of @bartosz-witkowski . In his implementation he introduced ```class(family)```, and worked with it in ```your``` - script. This approach might be good in English although it's inconvenient in Slovak. 

__Problems__ :

- when an English person talks about a member of his family he says ```my family member```. In Slovak there isn't any such convention, so Slovak talks about ```sister```, with meaning ```my sister```. Therefore I've introduced the ```family script``` called after detection of any family member. 
- in Slovak there are 3 grammatical genders : feminine, masculine and neuter
  - it means that we use different declinations of adjectives with words ( ```father```, ```brother``` ) and with words ( ```mother```, ```sister``` )

__Approaches__ :

- #### First approach :

  - ```class(family)``` matched in ```your``` script
  - it's affected by all the problems described above

- #### Second approach :

  - ```class(family)``` and different script for all its members implemented as ```scripts(Script) :- family(Member), Script=script(keyword(Member, 2),...)``` 

  - ##### Problems :

    - While I've resolved the problem with ```your``` in front of ```family member``` another problem emerged : 
      - ```findall``` predicate has to go through many more scripts in ```keyword_detection phase``` 
    - This doesn't handle any declinations at all.

  - ##### Possible solutions :

    - Introduce ```family``` script and handle ```keyword_detection``` phase similarly as in ```sorry_script``` 
  
- #### Third approach :

  - ```family``` keyword and ```family_masculine``` and ```family_feminine``` declination handling - this means that there are 2 possible ```adjective declinations``` . Example of communication :

    ​		```> otec nevie programovat``` (my father can't code)
    ​		``` Vas otec?``` (your father ?) <= in masculine
    ​		```> ani mama nevie programovat``` (my mother neither)
    ​		```Vasa mama ?```  (your mather ?) <= in feminine

  - ##### Problems :

    - recognition of grammatical genders in grammatical cases different from nominative