/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 */

package com.nawforce.apexlink.cst

import com.nawforce.pkgforce.path.Location
import com.nawforce.runtime.parsers.CodeParser
import io.github.apexdevtools.apexparser.ApexLexer
import org.antlr.v4.runtime.{CommonTokenStream, Token}

import scala.collection.mutable
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.util.matching.Regex

/** Parsed ApexDoc comment with extracted information */
case class ApexDocComment(
  location: Location,
  description: String,
  params: Map[String, String] = Map.empty,
  returnDescription: Option[String] = None,
  author: Option[String] = None,
  deprecated: Option[String] = None,
  tags: Map[String, String] = Map.empty
)

/** Parser for ApexDoc comments that extracts location and content information */
class ApexDocParser {

  /** Parse ApexDoc comments from token stream at specified token index
    * @param tokenStream the ANTLR token stream
    * @param tokenIndex the index of the token to search around
    * @return Option containing ApexDoc comment if found
    */
  def parseApexDocAt(tokenStream: CommonTokenStream, tokenIndex: Int): Option[ApexDocComment] = {
    // Look for ApexDoc comments to the left of the token (preceding the declaration)
    val tokensL = Option(tokenStream.getHiddenTokensToLeft(tokenIndex))
      .map(_.asScala.toSeq)
      .getOrElse(Seq.empty)
    
    // Look for ApexDoc comments to the right of the previous token
    val tokensR = if (tokenIndex > 0) {
      Option(tokenStream.getHiddenTokensToRight(tokenIndex - 1))
        .map(_.asScala.toSeq)
        .getOrElse(Seq.empty)
    } else Seq.empty

    val allTokens = tokensL ++ tokensR
    
    // Find the closest ApexDoc comment
    allTokens
      .filter(isApexDocComment)
      .sortBy(_.getTokenIndex)
      .lastOption
      .flatMap(parseApexDocToken)
  }

  /** Parse ApexDoc comments from source code using a CodeParser
    * @param parser the CodeParser instance
    * @param tokenIndex the token index to search around
    * @return Option containing ApexDoc comment if found
    */
  def parseApexDocFromParser(parser: CodeParser, tokenIndex: Int): Option[ApexDocComment] = {
    parser.lastTokenStream match {
      case Some(tokenStream) => parseApexDocAt(tokenStream, tokenIndex)
      case None => None
    }
  }

  /** Check if a token represents an ApexDoc comment */
  private def isApexDocComment(token: Token): Boolean = {
    val text = token.getText
    // Support both legacy /** */ format and new ApexDoc format
    text.startsWith("/**") || isNewApexDocFormat(text)
  }

  /** Check if token uses the new Salesforce ApexDoc format */
  private def isNewApexDocFormat(text: String): Boolean = {
    // Based on Salesforce documentation, new format may include specific markers
    // This is a placeholder for the actual new format detection
    // TODO: Update this when the new format specification is available
    text.startsWith("///") || text.contains("@apexdoc")
  }

  /** Parse an ApexDoc comment token into structured information */
  private def parseApexDocToken(token: Token): Option[ApexDocComment] = {
    val text = token.getText
    val location = Location(
      token.getLine,
      token.getCharPositionInLine,
      token.getLine + text.count(_ == '\n'),
      token.getCharPositionInLine + text.length
    )

    try {
      val cleanedText = cleanApexDocText(text)
      val (description, tags) = extractDescriptionAndTags(cleanedText)
      
      Some(ApexDocComment(
        location = location,
        description = description,
        params = extractParams(tags),
        returnDescription = extractTag(tags, "return"),
        author = extractTag(tags, "author"),
        deprecated = extractTag(tags, "deprecated"),
        tags = tags
      ))
    } catch {
      case _: Exception => None // Ignore malformed comments
    }
  }

  /** Clean ApexDoc comment text by removing comment markers */
  private def cleanApexDocText(text: String): String = {
    val lines = text.split('\n')
    val cleanedLines = lines.map { line =>
      line.trim
        .replaceFirst("^/\\*\\*", "") // Remove /** at start
        .replaceFirst("\\*/$", "")    // Remove */ at end
        .replaceFirst("^\\*", "")     // Remove leading * 
        .replaceFirst("^///", "")     // Remove /// for new format
        .trim
    }
    cleanedLines.mkString("\n").trim
  }

  /** Extract description and tags from cleaned comment text */
  private def extractDescriptionAndTags(text: String): (String, Map[String, String]) = {
    val tagPattern: Regex = """@(\w+)\s+(.*)""".r
    val lines = text.split('\n')
    val descriptionLines = mutable.ArrayBuffer[String]()
    val tags = mutable.Map[String, String]()
    
    var inDescription = true
    
    for (line <- lines) {
      line.trim match {
        case tagPattern(tagName, tagValue) =>
          inDescription = false
          tags(tagName) = tagValue.trim
        case other if inDescription && other.nonEmpty =>
          descriptionLines += other
        case _ => // Skip empty lines or continue with current mode
      }
    }
    
    (descriptionLines.mkString(" ").trim, tags.toMap)
  }

  /** Extract @param tags into a map */
  private def extractParams(tags: Map[String, String]): Map[String, String] = {
    tags.collect {
      case (key, value) if key == "param" =>
        // Parse "@param paramName description" format
        val parts = value.split("\\s+", 2)
        if (parts.length >= 2) {
          parts(0) -> parts(1)
        } else {
          parts(0) -> ""
        }
    }
  }

  /** Extract a specific tag value */
  private def extractTag(tags: Map[String, String], tagName: String): Option[String] = {
    tags.get(tagName).filter(_.nonEmpty)
  }
}

object ApexDocParser {
  /** Create a new ApexDoc parser instance */
  def apply(): ApexDocParser = new ApexDocParser()
  
  /** Parse ApexDoc comment from source at a specific location
    * @param source the source code
    * @param location the location to search around
    * @return Option containing ApexDoc comment if found
    */
  def parseAt(source: String, location: Location): Option[ApexDocComment] = {
    // This method would be used for on-demand parsing from stored locations
    // Implementation would create a parser and tokenize the source around the location
    // For now, this is a placeholder for the ApexDocProvider to use
    None // TODO: Implement when needed by ApexDocProvider
  }
}
