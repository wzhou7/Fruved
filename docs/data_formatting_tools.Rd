
### Step 1.1 Cereal Food Code

While the raw data has the two cereal choices coded as string (see [codebook](https://epi.grants.cancer.gov/nhanes/dietscreen/dsq_codebook_teleform.docx)),
before scoring, these strings need to be matched with the cereal database (`calib.DSQ.cereal.ntile`) to convert them into an 8-digit food code.
This step might require careful data cleaning. 
The following is an example to automatically do this conversion using our utility function `str2code()`.

```
data$DSQ_020 <- str2code(data$Dsqcertyp1)
data$DSQ_xx3 <- str2code(data$Dsqcertyp2)
```

### Step 1.2 Choice Encoding According to the Codebook

While in many cases the coded values for survey choices are natural numbers (e.g., `1` to `5` for a 5-choice question), 
the [codebook](https://epi.grants.cancer.gov/nhanes/dietscreen/dsq_codebook_teleform.docx) assumed that the choices are coded as alphabets. 
The following is an example to automatically do this conversion using our utility function `num2alpha()`.

```
data$DSQ_020 <- num2alpha(data$Dsqcertyp1)
data$DSQ_xx3 <- num2alpha(data$Dsqcertyp2)
```
