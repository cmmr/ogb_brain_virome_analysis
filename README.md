# ogb_brain_virome_analysis
Analysis repo for HVP OGB brain virome data.

## R Notebooks

Analyses and plots from manuscript can be recreated by running the R notebooks in `notebooks/`. I reommend opening the R project by double-clicking `ogb_brain_virome_analysis.Rproj` in the Finder.

## Charts

Charts that contributed to the manuscript are in `charts/`

## BLASTN command for B19 comparisons

```bash
blastn -query parvovirus_b19_genome_reconstructions1.1000nt.fna -db blast_DB -outfmt "6 qseqid sseqid pident length mismatch gaps qseq sseq" -out parvovirus_b19_genome_reconstructions1.1000nt.pairwise.blastn.seqs.tsv
```

