# Generate Authorship List with Numbered Affiliations
# Based on PFTC6 Excel data

library(dplyr)
library(stringr)

# Function to clean and format author names as "Last name middle initials. First name"
format_author_name <- function(full_name, last_name, middle_initials) {
  # Clean inputs
  full_name <- str_trim(full_name)
  last_name <- str_trim(last_name)
  middle_initials <- str_trim(middle_initials)
  
  # Extract first name from full name
  first_name <- str_split(full_name, "\\s+")[[1]][1]  # Take first word as first name
  
  # Format middle initials with dots
  if (!is.na(middle_initials) && middle_initials != "") {
    # Split middle initials and add dots
    middle_parts <- str_split(middle_initials, "")[[1]]  # Split each character
    middle_parts <- middle_parts[middle_parts != " "]  # Remove spaces
    middle_formatted <- paste0(middle_parts, ".", collapse = " ")
    full_name_formatted <- paste0(last_name, " ", middle_formatted, " ", first_name)
  } else {
    full_name_formatted <- paste0(last_name, " ", first_name)
  }
  
  return(full_name_formatted)
}

# Function to clean and format author names (old format for sample data)
format_author_name_old <- function(first_name, middle_names, last_name) {
  # Clean names
  first_name <- str_trim(first_name)
  middle_names <- str_trim(middle_names)
  last_name <- str_trim(last_name)
  
  # Format as "Last name (full), middle initials, first name (full)"
  if (!is.na(middle_names) && middle_names != "") {
    # If there are middle names, include their initials with dots
    middle_initials <- str_split(middle_names, "\\s+")[[1]] %>%
      str_sub(1, 1) %>%
      paste0(".", collapse = " ")
    full_name <- paste0(last_name, " ", middle_initials, " ", first_name)
  } else {
    full_name <- paste0(last_name, " ", first_name)
  }
  
  return(full_name)
}

# Function to get affiliation numbers for an author
get_affiliation_numbers <- function(affiliations, affiliation_mapping) {
  if (is.na(affiliations) || affiliations == "") {
    return(character(0))
  }
  
  # Split multiple affiliations if they exist
  affil_list <- str_split(affiliations, ";")[[1]] %>%
    str_trim() %>%
    .[. != ""]
  
  # Get numbers for each affiliation
  numbers <- sapply(affil_list, function(affil) {
    if (affil %in% names(affiliation_mapping)) {
      return(affiliation_mapping[affil])
    } else {
      return(NA)
    }
  })
  
  # Remove NAs and sort
  numbers <- numbers[!is.na(numbers)] %>%
    sort() %>%
    unique()
  
  return(numbers)
}

# Function to format affiliation numbers
format_affiliation_numbers <- function(numbers) {
  if (length(numbers) == 0) {
    return("")
  } else if (length(numbers) == 1) {
    return(as.character(numbers))
  } else {
    return(paste(numbers, collapse = ","))
  }
}

# Function to create author order mapping
create_author_order <- function() {
  # Define the specific author order as provided
  author_order <- c(
    "Vandvik Vigdis", "Halbritter H. Aud", "Macias-Fauria Marc", "Maitner S. Brian", 
    "Michaletz T. Sean", "Telford J. Richard", "Bison Nicolas", "Chacon-Labella Julia", 
    "Cotner Sehoya", "Egelkraut D", "Garen J", "Gaudard J", "Geange S. R.", 
    "Rosati M. R.", "Andersen E. A. S.", "Ahler S. J.", "Atkinson J", "Baumane M", 
    "Bradler P", "Dawson H. R.", "Eckberg J", "Elsy A. D.", "Erkelenz J", 
    "Eshelman S. E.", "Guclu C", "Gullvåg R", "Gya R", "Hartford S", 
    "Hayden M. T.", "Holle M. J. M.", "Kullberg A. T.", "Lepley K", "Correia M", 
    "Löwenstein C. E.", "Maré C", "Mauki D", "Navarro J", "Oberholzer B", 
    "Olivier B", "Olson A. N.", "Ray C. A.", "von Oppen J", "Vorstenbosch T", 
    "Wang J. A.", "Enquist B. J."
  )
  
  # Create mapping with order numbers
  order_mapping <- setNames(seq_along(author_order), author_order)
  
  return(order_mapping)
}

# Main function to generate authorship list
generate_authorship_list <- function(data, custom_order = TRUE) {
  # First, order the authors according to the custom order
  if (custom_order) {
    author_order_mapping <- create_author_order()
    
    # Add order column based on the custom order
    data_ordered <- data %>%
      mutate(
        order_rank = sapply(if ("FullName" %in% names(data)) FullName else paste(FirstName, LastName), function(name) {
          if (name %in% names(author_order_mapping)) {
            return(author_order_mapping[name])
          } else {
            return(999) # Put unknown authors at the end
          }
        })
      ) %>%
      arrange(order_rank)
  } else {
    # Default alphabetical ordering
    data_ordered <- data %>%
      arrange(LastName, FirstName)
  }
  
  # Now create affiliation mapping based on the ordered authors
  # Process affiliations in the order they appear in the ordered author list
  affiliation_list <- c()
  
  for (i in 1:nrow(data_ordered)) {
    author_affiliations <- c(data_ordered$Address1[i], data_ordered$OtherAddress[i], data_ordered$OtherAddress2[i]) %>%
      na.omit() %>%
      str_trim() %>%
      .[. != ""]
    
    # Add new affiliations to the list
    for (affil in author_affiliations) {
      if (!(affil %in% affiliation_list)) {
        affiliation_list <- c(affiliation_list, affil)
      }
    }
  }
  
  # Create sequential numbering for affiliations (1, 2, 3, etc.)
  affiliation_mapping <- setNames(seq_along(affiliation_list), affiliation_list)
  
  # Process each author using the ordered data
  authors_with_affiliations <- data_ordered %>%
    filter(!is.na(LastName)) %>%
    rowwise() %>%
    mutate(
      # Format author name using the new structure
      formatted_name = if ("FullName" %in% names(data_ordered) && "MiddleInitials" %in% names(data_ordered)) {
        format_author_name(FullName, LastName, MiddleInitials)
      } else {
        # Fallback for sample data or old format
        format_author_name_old(FirstName, MiddleNames, LastName)
      },
      
      # Get affiliation numbers
      affil_numbers = list(get_affiliation_numbers(Address1, affiliation_mapping)),
      other_affil_numbers = list(get_affiliation_numbers(OtherAddress, affiliation_mapping)),
      other2_affil_numbers = list(get_affiliation_numbers(OtherAddress2, affiliation_mapping)),
      
      # Combine all affiliation numbers
      all_affil_numbers = list(unique(c(unlist(affil_numbers), 
                                       unlist(other_affil_numbers), 
                                       unlist(other2_affil_numbers)))),
      
      # Format affiliation numbers
      affiliation_string = format_affiliation_numbers(unlist(all_affil_numbers)),
      
      # Create final author string
      author_string = if(affiliation_string != "") {
        paste0(formatted_name, affiliation_string)
      } else {
        formatted_name
      }
    ) %>%
    ungroup()
  
  # Create the final authorship list
  authorship_list <- paste(authors_with_affiliations$author_string, collapse = ", ")
  
  # Create affiliation list
  affiliation_list <- data.frame(
    Number = seq_along(affiliation_mapping),
    Affiliation = names(affiliation_mapping)
  )
  
  return(list(
    authorship = authorship_list,
    affiliations = affiliation_list,
    authors_data = authors_with_affiliations
  ))
} 