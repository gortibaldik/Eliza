# Lemmatization and stemming

- its considerably harder to detect keywords in Slovak than in English mainly because of declinations of nouns and conjugation of verbs

I've chosen the 'sorry' script as an example of transformations applied

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

