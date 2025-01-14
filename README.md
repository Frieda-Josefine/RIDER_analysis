<h2> :bar_chart: <strong>RIDER Analysis Repository</strong></h2>


This repository contains the scripts used for the statistical analysis and plotting in the paper: <strong>Long-Term Effects of Working Memory Retrieval From Prioritized and Deprioritized States</strong>. You find separate folders for each of the three RIDER experiments (1/2/3). Below is an overview of the folder structure and the contents.</p>

<hr>

<h3> :mechanical_arm: <strong>Folder Overview</strong></h3>

<ul>
  <li> :bar_chart: <strong><code>Plots:</code></strong> This folder contains all the plotting scripts used to generate the figures and visualizations for the RIDER manuscript.</li>

  <li> :file_folder: <strong><code>RIDER1:</code></strong> This folder contains two subfolders:
    <ul>
      <li> :memo: <strong><code>statistics:</code></strong> Contains the statistical analysis scripts for RIDER1. These scripts perform the necessary calculations and tests to analyze the experimental data.</li>
      <li> :page_facing_up: <strong><code>docs:</code></strong> Contains:
        <ul>
          <li> A Markdown document that provides an easy-to-read overview of the results generated by the statistical scripts. This allows for quick access to the findings without running the scripts.</li>
          <li> &#128213; The <strong><code>variable codebook</code></strong>. (Please note, that this codebook should give an overview for all three experiments, but it can be found here in the RIDER1 folder.)</li>
        </ul>
      </li>
    </ul>
</li>
  <li> :file_folder: <strong><code>RIDER2/3:</code></strong> These folders are structured identically to the <strong>RIDER1</strong> folder, each containing:
    <ul>
      <li> :memo: <strong><code>statistics:</code></strong> Statistical analysis scripts for RIDER2/3.</li>
      <li> :page_facing_up: <strong><code>docs:</code></strong> Markdown files summarizing the results for easy reference without running the scripts.</li>
    </ul>
  </li>

  <li> :wrench: <strong><code>src:</code></strong> This folder contains the necessary scripts to set up the environment for the analysis, ensuring all dependencies and configurations are in place for smooth execution of the statistical and plotting scripts &#128209; and a folder for the scripts that select the variables essential for analysis from the raw data (without any processing).</li>
</ul>

<hr>

<h3> :memo: <strong>Usage Instructions</strong></h3>

<p>If you want to go through the analysis of our manuscript (Long-Term Effects of Working Memory Retrieval From Prioritized and Deprioritized States
) you can follow these steps:</p>
<ol>
  <li> Clone this repository to your local machine.</li>
  <li> Download the raw data from our GIN repository.</li>
  <li> Navigate to the <code>src</code> folder and make sure the utilities script is in place for the environment setup and to ensure all dependencies are installed.</li>
  <li> Use the statistical scripts located in <code>RIDER1</code>, <code>RIDER2</code>, and <code>RIDER3</code> to perform analysis, or simply refer to the Markdown documents for a summary of the results.</li>
  <li> Make sure to change the paths in each of the scripts to load the data from the respective location on you computer that holds the raw data tables.</li>
  <li> Use the plotting scripts in the <code>Plots</code> folder to generate the figures and visualizations required for the manuscript.</li>
</ol>

<p align="right">(<a href="#readme-top">back to top</a>)</p>
