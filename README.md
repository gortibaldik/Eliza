# ELIZA CHATBOT

According to [wikipedia](https://en.wikipedia.org/wiki/ELIZA), Eliza is an early natural language processing computer program simulating a conversation. I attempted to recreate intended behaviors of Eliza so that it'll be able to communicate in Slovak.

## Utilization

Every line of code was developed and tested using ```SWI - Prolog``` version ```8.0.3 64-bit```. All the pieces of code are intended to be used all together and they're in separate files only for better readibility.

- The main part, the _conversation tool_ is in predicate ```eliza/0```.  It holds a conversation with user in Slovak until it spots line with single word : ```keyword``` "dovidenia" or until user hasn't used obscenities more than 3 times. 

- Other useful tools include: 
  - predicate ```conj(+Verb, -Person, -Time)``` which returns the conjugation of ```Verb```  
    - (```Time``` = ```now``` , ```Person``` in {```sg1```, ```sg2```, ```sg3```, ```pl1```, ```pl2```, ```pl3```})
    - (```Time``` = ```past```, ```Person``` in {```sg```, ```pl```})
    - (```Time``` = ```neuter```, ```Person``` = ```neuter```)
  - predicate ```read_atomics/1``` which reads the standard input, divides it by whitespaces, lowercases it and returns as a ```list``` of ```atoms```.
  - predicate ```comment/1``` which writes its argument, which has to be ```list``` of ```atoms``` to standard output

## Example runs of ```eliza/0``` 

### Example run from [paper](http://web.stanford.edu/class/cs124/p36-weizenabaum.pdf)

> __Dobry den porozpravajte mi o vasom probleme!__\
> \> vsetci muzi su rovnaki\
> __Akym sposobom?__\
> \> stale nas s niecim otravuju\
> __Viete vymysliet nejaky konkretny priklad?__\
> \> napriklad, moj priatel ma prinutil prist sem\
> __Vas priatel vas prinutil prist sem?__\
> \> hovori, ze som stale depresivna\
> __Je mi luto, ked pocujem, ze ste depresivna__\
> \> je to pravda, som nestastna\
> __Myslite si, ze ked ste nestastna tak vam pomoze rozpravat sa so mnou?__\
> \> potrebujem nejaku pomoc, to je iste\
> __Preco chcete nejaku pomoc?__\
> \> pretoze sa chcem pohodnut s mojou mamou\
> __Rozpravate sa casto s mamou?__\
> \> ano, moja mama sa o mna stara\
> __Je este niekto vo vasej rodine kto sa o vas stara?__\
> \> moj otec\
> __Vas otec?__\
> \> obcas sa podobas na mojho otca, Eliza\
> __Vidite nejake spolocne vlastnosti?__\
> \> nie si velmi agresivna ale myslim, ze len nechces, aby som si to vsimla\
> __Hovorite nie len preto aby ste zneli nastvane?__\
> \> dovidenia\
> __Dakujem za podnetny rozhovor. Dufam, ze sa este niekedy stretneme.__



### Example run showing handling of declensions and conjugations in feel script

> __Dobry den porozpravajte mi o vasom probleme!__\
> \> citim sa uplne strasne\
> __Povedzte mi viac o tychto pocitoch__\
> \> citim sa ako na horskej drahe\
> __Ako casto sa citite ako na horskej drahe?__\
> \> citim si nohy\
> __Uzivate si ked si citite nohy?__\
> \> citim sa ako v deliriu\
> __Aky pocit vam to pripomina?__\
> \> dovidenia\
> __Dakujem za podnetny rozhovor. Dufam, ze sa este niekedy stretneme.__

### Example run showing handling of obscenities

> __Dobry den porozpravajte mi o vasom probleme!__\
> \> jebat tento svet\
> __Poprosim vas nenadavajte v tejto konverzacii__\
> \> ale chod do pice\
> __Mozno vam to je prirodzene, ale upozornujem vas nenadavajte mi tu__\
> \> vies co si ? obycajna svina !\
> __Posledny krat vas ziadam nenadavajte mi tu!__\
> \> ty kurva, co ma upozornujes ?\
> __Chod do prdele ty drbo!__



## How does it work ?

I'll show workflow of predicate ```eliza/0```, which is the main component of this semestral project.

- At the start of the predicates workflow Eliza prompts user to start conversation with script from ```start_script/1```, and prints it to standard output with ```comment/1``` (```start_script``` is simple fact containing beginning message of conversation, ```comment``` prints ```list``` of atoms gotten from ```start_script``` to standard output)

- ```read_atomics/1``` reads the standard input and breaks it into ```list``` of ```atoms``` which is processed by ```get_comment``` 

- ### Workflow of get_comment :

  - ```traverse_input_stem_lemm(+User_input, -Stemmed_lemmed)```  : 
    - Since in Slovak there are many different synonyms, conjugations and declensions at first we need to :
      - unify some kind of words (```stale``` -> ```vzdy``` {```every_time``` ->```always```})
      - transform possessives (```moj``` -> ```tvoj``` {```my``` -> ```your```}) so that we won't have to worry about these later in processing of input (because when user talks about something belonging to him, we want to query it, so we need these transformations)
      - transform conjugations (```robim``` -> ```robite``` {```I do``` -> ```you do``` })  for the same reasons as above
  - ```get_scripts_matching_keywords(+Stemmed_input, -Scripts)``` 
    - In this phase we traverse input for the second time and use all the conditional transformations to find the keywords
    - "Conditional Transformation" : concept of matching when we don't actually change user input, but we look at it as on similar keyword
      - for example when user says ```I'm depressed``` we should look at it as on ```I'm sad``` because the meanings are similar, although we don't transform ```depressed``` -> ```sad``` because the meanings aren't same
  - now there are two possibilites how to continue based on output of ```get_scripts_matching_keywords/2```, they are basically the same, so I'll unify their description even though they are in separate code scopes
  - ```get_initial_uninformed_memory_comment(-Output, -Keyword, -Pattern_index, -Priority)``` - get last entry in ```memory/1```, fail if there is only empty list in ```memory/1``` ([How does ```memory/1``` work ?](#memory-concept))





### Memory concept :

  - 




# Lemmatization and stemming

- its considerably harder to detect keywords in Slovak than in English mainly because of declension of nouns and conjugation of verbs

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
  - it means that we use different declensions of adjectives with words ( ```father```, ```brother``` ) and with words ( ```mother```, ```sister``` )

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
    

- #### Forth approach :

  - introduction of check of grammatical cases in ```eliza_language_utils.pl```, after detection of any ```family member``` we also detect its grammatical case with aid of ```gram_case_masculine_sg``` and ```gram_case_feminine_sg```

  - introduction of patterns handling matched grammatical cases through ```class(family_feminine, Word, Case)``` and ```class(family_masculine, Word, Case)``` 

  - many times we need to force presence of another word after the keyword -> introduction of ```class(atom, X)``` to be able to query appropriately 

  - This is the final approach with following results :

    - ```> sestre nedochadza, ze to nie je pravda``` (my sister doesn't realize that it isn't true)
    - ```Je este niekto vo vasej rodine komu nedochadza, ze to nie je pravda?``` (is there anybody else in your family, who doesn't realize that it isn't true ?)
    - ```> s mojou mamkou sme sa dneska ucili molove stupnice``` (Today we were practicing minor scales with my mother.)
    - ```> Rozpravate sa casto s mamkou?``` (Do you speak often with your mother ?)



### Conjugation

- predicate ```conjugation(Verb, Number, Time, Base)``` which based on ```Verb ```finds ```Number``` (```sg```,```pl```), ```Time``` (```now```, ```past```, ```neuter```)
- basic transformations
  -  ```sg1``` -> ```pl2``` ( when user speaks about himself, we want to ask him questions (and use polite form (that's why we don't use ```sg2``` instead of ```pl2```)))
  - ```pl2``` -> ```sg1``` | ```sg2``` -> ```pl1``` (same reasons as above)

TODO : How does conjugation work, which predicates does it use

TODO : How does Eliza matching process works ?