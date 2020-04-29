campaign.create <-
  function(`Campaign`,
           `Budget`,
           `AdGroups`,
           `Keywords`,
           `Ads`,
           `Locations`,
           `Ad rotation` = "Rotate indefinitely",
           `Ad Schedule` = "[]",
           `Bid Strategy Type` = "Manual CPC",
           `Budget type` = "Daily",
           `Campaign Priority` = "Low",
           `Campaign Status` = "Enabled",
           `Campaign Type` = "Search",
           `Delivery method` = "Standard",
           `DSA targeting source` = "Google",
           `End Date` = "[]",
           `Enhanced CPC` = "Disabled",
           `Exclusion method` = "Location of presence",
           `Flexible Reach` = "Audiences",
           `Inventory filter` = "*",
           `Languages` = "de",
           `Local Inventory Ads` = "Disabled",
           `Merchant Identifier` = "0",
           `Networks` = "Google search",
           `Start Date` = "[]",
           `Targeting method` = "Location of presence or Area of interest") {
    dplyr::tibble(
      `Ad rotation` = c(`Ad rotation`),
      `Ad Schedule` = c(`Ad Schedule`),
      `Bid Strategy Type` = c(`Bid Strategy Type`),
      `Budget` = c(`Budget`),
      `Budget type` = c(`Budget type`),
      `Campaign` = c(`Campaign`),
      `Campaign Priority` = c(`Campaign Priority`),
      `Campaign Status` = c(`Campaign Status`),
      `Campaign Type` = c(`Campaign Type`),
      `Delivery method` = c(`Delivery method`),
      `DSA targeting source` = c(`DSA targeting source`),
      `End Date` = c(`End Date`),
      `Enhanced CPC` = c(`Enhanced CPC`),
      `Exclusion method` = c(`Exclusion method`),
      `Flexible Reach` = c(`Flexible Reach`),
      `Inventory filter` = c(`Inventory filter`),
      `Languages` = c(`Languages`),
      `Local Inventory Ads` = c(`Local Inventory Ads`),
      `Merchant Identifier` = c(`Merchant Identifier`),
      `Networks` = c(`Networks`),
      `Start Date` = c(`Start Date`),
      `Targeting method` = c(`Targeting method`)
    ) %>%
      dplyr::bind_rows(`AdGroups`) %>%
      dplyr::bind_rows(`Keywords`) %>%
      dplyr::bind_rows(`Ads`) %>%
      dplyr::bind_rows(`Locations`) %>%
      replace(is.na(.), "")
  }

ad.group.create <-
  function(`Ad Group`,
           `Campaign`,
           `Max CPC`,
           `Ad Group Status` = "Enabled",
           `Ad Group Type` = "Default",
           `Campaign Status` = "Enabled",
           `Display Network Custom Bid Type` = "None",
           `Flexible Reach` = "Audiences;Genders;Ages;Parental status;Household incomes",
           `Targeting expansion` = "Level 1") {
    dplyr::tibble(
      `Ad Group` = c(`Ad Group`),
      `Ad Group Status` = c(`Ad Group Status`),
      `Ad Group Type` = c(`Ad Group Type`),
      `Campaign` = c(`Campaign`),
      `Campaign Status` = c(`Campaign Status`),
      `Display Network Custom Bid Type` = c(`Display Network Custom Bid Type`),
      `Flexible Reach` = c(`Flexible Reach`),
      `Max CPC` = c(`Max CPC`),
      `Targeting expansion` = c(`Targeting expansion`)
    )
  }

keyword.create <-
  function(`Keyword`,
           `Criterion Type`,
           `Ad Group`,
           `Campaign`,
           `Ad Group Status` = "Enabled",
           `Ad relevance` = "-",
           `Approval Status` = "Pending review",
           `Campaign Status` = "Enabled",
           `Expected CTR` = " -",
           `First page bid` = "0",
           `First position bid` = "0",
           `Landing page experience` = " -",
           `Status` = "Enabled",
           `Top of page bid` = "0") {
    dplyr::tibble(
      `Ad Group` = c(`Ad Group`),
      `Ad Group Status` = c(`Ad Group Status`),
      `Ad relevance` = c(`Ad relevance`),
      `Approval Status` = c(`Approval Status`),
      `Campaign` = c(`Campaign`),
      `Campaign Status` = c(`Campaign Status`),
      `Criterion Type` = c(`Criterion Type`),
      `Expected CTR` = c(`Expected CTR`),
      `First page bid` = c(`First page bid`),
      `First position bid` = c(`First position bid`),
      `Keyword` = c(`Keyword`),
      `Landing page experience` = c(`Landing page experience`),
      `Status` = c(`Status`),
      `Top of page bid` = c(`Top of page bid`)
    )
  }

.create.phrase.match.phrase <- function(keyword) {
  "{keyword} -[{keyword}]" %>% glue::glue()
}

.create.broad.match.phrase <- function(keyword) {
  keyword %>% stringr::str_split(' ') %>% purrr::map(~ paste0("+", ., collapse = " ")) %>% unlist()
}

keyword.create.match.types <-
  function(`Keyword`,
           `Ad Group`,
           `Campaign`,
           `Ad Group Status` = "Enabled",
           `Ad relevance` = "-",
           `Approval Status` = "Pending review",
           `Campaign Status` = "Enabled",
           `Expected CTR` = " -",
           `First page bid` = "0",
           `First position bid` = "0",
           `Landing page experience` = " -",
           `Status` = "Enabled",
           `Top of page bid` = "0") {
    keyword.create(
      `Keyword`,
      "Exact",
      `Ad Group`,
      `Campaign`,
      `Ad Group Status`,
      `Ad relevance`,
      `Approval Status`,
      `Campaign Status`,
      `Expected CTR` ,
      `First page bid`,
      `First position bid`,
      `Landing page experience`,
      `Status`,
      `Top of page bid`
    ) %>%
      dplyr::bind_rows(
        keyword.create(
          .create.phrase.match.phrase(`Keyword`),
          "Phrase",
          `Ad Group`,
          `Campaign`,
          `Ad Group Status`,
          `Ad relevance`,
          `Approval Status`,
          `Campaign Status`,
          `Expected CTR` ,
          `First page bid`,
          `First position bid`,
          `Landing page experience`,
          `Status`,
          `Top of page bid`
        )
      ) %>%
      dplyr::bind_rows(
        keyword.create(
          .create.broad.match.phrase(`Keyword`),
          "Broad",
          `Ad Group`,
          `Campaign`,
          `Ad Group Status`,
          `Ad relevance`,
          `Approval Status`,
          `Campaign Status`,
          `Expected CTR` ,
          `First page bid`,
          `First position bid`,
          `Landing page experience`,
          `Status`,
          `Top of page bid`
        )
      )

  }

ad.create <-
  function(`Headline 1`,
           `Headline 2`,
           `Headline 3`,
           `Description Line 1`,
           `Description Line 2`,
           `Final URL`,
           `Path 1`,
           `Path 2`,
           `Campaign`,
           `Ad Group`,
           `Status` = "Enabled",
           `Ad Group Status` = "Enabled",
           `Ad type` = "Expanded text ad",
           `Approval Status` = "Pending review",
           `Campaign Status` = "Enabled") {
    dplyr::tibble(
      `Ad Group` = c(`Ad Group`),
      `Ad Group Status` = c(`Ad Group Status`),
      `Ad type` = c(`Ad type`),
      `Approval Status` = c(`Approval Status`),
      `Campaign` = c(`Campaign`),
      `Campaign Status` = c(`Campaign Status`),
      `Description Line 1` = c(`Description Line 1`),
      `Description Line 2` = c(`Description Line 2`),
      `Final URL` = c(`Final URL`),
      `Headline 1` = c(`Headline 1`),
      `Headline 2` = c(`Headline 2`),
      `Headline 3` = c(`Headline 3`),
      `Path 1` = c(`Path 1`),
      `Path 2` = c(`Path 2`),
      `Status` = c(`Status`)
    )
  }

location.create <-
  function(`ID`,
           `Location`,
           `Reach`,
           `Campaign`,
           `Campaign Status` = "Enabled",
           `Status` = "Enabled") {
    dplyr::tibble(
      `Campaign` = c(`Campaign`),
      `Campaign Status` = c(`Campaign Status`),
      `ID` = c(`ID`),
      `Location` = c(`Location`),
      `Reach` = c(`Reach`),
      `Status` = c(`Status`)
    )
  }
